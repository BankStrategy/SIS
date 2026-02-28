{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | DGM.HsAST — Real Haskell AST module via ghc-exactprint (SELFMOD.md Phase B).
--
-- Wraps @ghc-exactprint@ to parse, traverse, and mutate real @.hs@ source
-- files with round-trip fidelity.  The rest of the system is insulated from
-- raw GHC types behind an opaque @'HsModule'@ handle.
--
-- Compiled unconditionally: when the @with-exactprint@ cabal flag is absent,
-- every function returns a @'Left'@ stub explaining the backend is disabled.
module DGM.HsAST
  ( -- * Parsing
    HsModule
  , parseHsFile
  , parseHsText
    -- * Printing (exact round-trip)
  , printHsModule
    -- * Traversal
  , HsNode(..)
  , hsNodes
  , hsNodeText
    -- * Mutations
  , HsMutation(..)
  , applyHsMutation
  , collectHsMutations
    -- * Rule sets
  , HsRewriteRule
  , HsRuleSet
  , defaultHsRules
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isLower)
import Data.Maybe (mapMaybe)

#ifdef WITH_EXACTPRINT
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EPP
import qualified GHC (ParsedSource)
import Control.Exception (bracket, try, SomeException)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.IO (writeFile)
import System.Random (randomRIO)

-- ─────────────────────────────────────────────────────────────────────────────
-- Core types (real implementation)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Opaque handle to a parsed Haskell module.
--
-- 'HsModuleParsed' carries the @ghc-exactprint@ @ParsedSource@ with all
-- formatting annotations needed for exact-fidelity reprinting.
-- 'HsModuleMutated' stores a post-mutation text representation (the result of
-- applying a 'HsMutation'); it is not yet re-parsed.
data HsModule
  = HsModuleParsed GHC.ParsedSource
    -- ^ Fresh from 'parseHsFile' or 'parseHsText'.
  | HsModuleMutated !Text
    -- ^ After 'applyHsMutation'; call 'parseHsText' to re-parse.

-- | A node in the Haskell source, identified by kind and display text.
data HsNode = HsNode
  { hnText :: !Text  -- ^ Identifier or module name introduced by this node.
  , hnKind :: !Text  -- ^ @"import"@, @"value"@, @"type"@, etc.
  , hnLine :: !Int   -- ^ 1-based source line (0 = unknown).
  } deriving (Show, Eq)

-- | A proposed source-level mutation (pure text transform).
--
-- The transform operates on the exact-printed text of the module.
-- 'applyHsMutation' calls it and wraps the result in 'HsModuleMutated'.
data HsMutation = HsMutation
  { hmDescription :: !Text
  , hmTransform   :: Text -> Either Text Text
    -- ^ @Left err@ if inapplicable; @Right newSrc@ on success.
  }

-- | Alias for rule-set clarity.
type HsRewriteRule = HsMutation

-- | An ordered collection of rewrite rules.
type HsRuleSet = [HsRewriteRule]

-- ─────────────────────────────────────────────────────────────────────────────
-- Parsing
-- ─────────────────────────────────────────────────────────────────────────────

-- | Parse a @.hs@ source file, preserving all exact-print annotations.
--
-- Returns @'Left' err@ on parse failure.
parseHsFile :: FilePath -> IO (Either Text HsModule)
parseHsFile fp = do
  result <- EPP.parseFile fp
  pure $ case result of
    Left (_, errMsg) -> Left (T.pack errMsg)
    Right ps         -> Right (HsModuleParsed ps)

-- | Parse Haskell source text.
--
-- Writes @src@ to a uniquely-named temporary @.hs@ file, calls 'parseHsFile',
-- then removes the temp file.  The temp file has a @.hs@ extension so that
-- GHC's parser recognises it as Haskell source.
parseHsText :: Text -> IO (Either Text HsModule)
parseHsText src = do
  tmpDir <- getTemporaryDirectory
  n <- randomRIO (0 :: Int, maxBound)
  let fp = tmpDir </> ("DGM-hsast-" ++ show n ++ ".hs")
  bracket
    (System.IO.writeFile fp (T.unpack src) >> pure fp)
    (\f -> try (removeFile f) >>= \(_ :: Either SomeException ()) -> pure ())
    parseHsFile

-- ─────────────────────────────────────────────────────────────────────────────
-- Printing
-- ─────────────────────────────────────────────────────────────────────────────

-- | Reconstruct source text.
--
-- For 'HsModuleParsed', uses @ghc-exactprint@'s 'EP.exactPrint' to reproduce
-- the original source with full fidelity.  For 'HsModuleMutated', returns the
-- stored mutation result directly.
printHsModule :: HsModule -> Text
printHsModule (HsModuleParsed ps)  = T.pack (EP.exactPrint ps)
printHsModule (HsModuleMutated t)  = t

-- ─────────────────────────────────────────────────────────────────────────────
-- Traversal
-- ─────────────────────────────────────────────────────────────────────────────

-- | Extract a flat list of top-level AST nodes.
--
-- Uses text-level pattern matching rather than the GHC API so the function
-- is stable across GHC minor versions.  Returns import declarations and
-- top-level value/type bindings.  For 'HsModuleMutated' (post-mutation text),
-- the same text-level extraction is applied to the stored source.
hsNodes :: HsModule -> [HsNode]
hsNodes m =
  let src = printHsModule m
      ls  = zip [1..] (T.lines src)
  in  concatMap extractNode ls
  where
    extractNode :: (Int, Text) -> [HsNode]
    extractNode (lineNum, line)
      | "import " `T.isInfixOf` T.stripStart line =
          [HsNode { hnText = extractModName line
                  , hnKind = "import"
                  , hnLine = lineNum }]
      | not (T.null (T.strip line))
        , Just (lhs, _) <- splitAtBinding line
        , (name:_) <- T.words lhs
        , not (T.null name)
        , isLower (T.head name) =
          [HsNode { hnText = name, hnKind = "value", hnLine = lineNum }]
      | otherwise = []

    extractModName :: Text -> Text
    extractModName line =
      case T.words (T.strip line) of
        ("import":rest) ->
          let ws = dropWhile (`elem` (["qualified", "safe", "hiding"] :: [Text])) rest
          in case ws of
               (m:_) -> T.takeWhile (\c -> isAlphaNum c || c == '.' || c == '_') m
               []    -> ""
        _ -> ""

-- | Get the display text of a node (the identifier or module name it introduces).
hsNodeText :: HsNode -> Text
hsNodeText = hnText

-- ─────────────────────────────────────────────────────────────────────────────
-- Mutations
-- ─────────────────────────────────────────────────────────────────────────────

-- | Apply a single mutation to a module (pure — no file I/O).
--
-- Returns @'Left' err@ if the mutation is not applicable to this module.
-- The result is an 'HsModuleMutated' carrying the new source text; call
-- 'parseHsText' to re-parse it into a fully-annotated 'HsModule'.
applyHsMutation :: HsMutation -> HsModule -> Either Text HsModule
applyHsMutation mut m =
  case hmTransform mut (printHsModule m) of
    Left err  -> Left err
    Right src -> Right (HsModuleMutated src)

-- | Collect all mutations that apply to the given module.
--
-- A rule is included only if its transform returns @'Right'@ AND produces a
-- text different from the original (no-op rewrites are discarded).
collectHsMutations :: HsRuleSet -> HsModule -> [HsMutation]
collectHsMutations rules m =
  let src = printHsModule m
  in  mapMaybe (tryRule src) rules
  where
    tryRule src rule =
      case hmTransform rule src of
        Left _               -> Nothing
        Right src' | src' == src -> Nothing
        Right _              -> Just rule

-- ─────────────────────────────────────────────────────────────────────────────
-- Default rule set
-- ─────────────────────────────────────────────────────────────────────────────

-- | Conservative rule set for Phase B self-modification.
--
-- Includes eta-reduction and unused-import removal.  No rules that change
-- type signatures or introduce new definitions.
defaultHsRules :: HsRuleSet
defaultHsRules =
  [ etaReduceRule
  , unusedImportRule
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Rule: eta-reduction
-- ─────────────────────────────────────────────────────────────────────────────

-- | Eta-reduce top-level function definitions of the form @f x = g x → f = g@.
--
-- Detects the syntactic pattern where the last argument on the LHS also appears
-- as the last token on the RHS, and does not appear elsewhere in the RHS.
-- Only the first such definition in the file is rewritten per invocation.
etaReduceRule :: HsMutation
etaReduceRule = HsMutation
  { hmDescription = "eta-reduce: f x = g x \x2192 f = g"
  , hmTransform   = etaReduceTransform
  }

etaReduceTransform :: Text -> Either Text Text
etaReduceTransform src =
  let ls      = zip [0..] (T.lines src)
      results = mapMaybe tryEtaReduce ls
  in  case results of
        []              -> Left "no eta-reducible definitions found"
        (idx, newLine) : _ ->
          let original = T.lines src
              patched  = take idx original ++ [newLine] ++ drop (idx + 1) original
          in  Right (T.unlines patched)
  where
    tryEtaReduce :: (Int, Text) -> Maybe (Int, Text)
    tryEtaReduce (idx, line)
      -- Skip comments, type signatures, blank lines, and module/where lines.
      | T.null (T.strip line)                        = Nothing
      | "--" `T.isPrefixOf` T.stripStart line        = Nothing
      | "::" `T.isInfixOf` line                      = Nothing
      | "module " `T.isPrefixOf` T.stripStart line   = Nothing
      | otherwise =
          case splitAtBinding line of
            Nothing        -> Nothing
            Just (lhs, rhs) ->
              let lhsWds = T.words lhs
                  rhsWds = T.words (T.strip rhs)
              in  if length lhsWds >= 2 && length rhsWds >= 2
                  then
                    let lastLhs = last lhsWds
                        lastRhs = last rhsWds
                    in  if lastLhs == lastRhs
                           && isSimpleVar lastLhs
                           -- Arg must not appear in the rest of the RHS body.
                           && not (lastLhs `T.isInfixOf` T.unwords (init rhsWds))
                        then
                          let newLhs  = T.unwords (init lhsWds)
                              newRhs  = T.unwords (init rhsWds)
                              indent  = T.takeWhile (== ' ') line
                          in  Just (idx, indent <> newLhs <> " = " <> newRhs)
                        else Nothing
                  else Nothing

-- ─────────────────────────────────────────────────────────────────────────────
-- Rule: unused import removal
-- ─────────────────────────────────────────────────────────────────────────────

-- | Remove explicit-import entries whose listed names do not appear in the
-- module body (heuristic: name occurrence search in source text).
--
-- Only explicit import lists (@import M (f, g)@) are analysed; qualified and
-- wildcard imports are left untouched.
unusedImportRule :: HsMutation
unusedImportRule = HsMutation
  { hmDescription = "remove unused explicit imports"
  , hmTransform   = unusedImportTransform
  }

unusedImportTransform :: Text -> Either Text Text
unusedImportTransform src =
  let ls       = T.lines src
      -- Separate import block from the rest of the file.
      (pre, rest) = break (not . isImportOrBlank) ls
      body       = T.unlines rest
      -- Identify import lines that have explicit lists where none of the
      -- listed names appears in the body.
      toRemove   = filter (isUnusedExplicitImport body) pre
  in  if null toRemove
      then Left "no unused explicit imports detected"
      else Right $ T.unlines $ filter (`notElem` toRemove) ls
  where
    isImportOrBlank :: Text -> Bool
    isImportOrBlank l =
      T.null (T.strip l) || "import " `T.isPrefixOf` T.stripStart l

    -- | True if this line is an explicit import (@import M (names)@) AND
    -- none of the listed bare names appear in the file body.
    isUnusedExplicitImport :: Text -> Text -> Bool
    isUnusedExplicitImport body line =
      case extractExplicitNames line of
        Nothing    -> False  -- not an explicit import
        Just []    -> False  -- empty list — keep for side effects
        Just names -> all (\n -> not (n `T.isInfixOf` body)) names

    -- | Extract the names from @import M (f, g, ...)@.
    -- Returns @Nothing@ for qualified / hiding / wildcard imports.
    extractExplicitNames :: Text -> Maybe [Text]
    extractExplicitNames line =
      let stripped = T.strip line
      in  if not ("import " `T.isPrefixOf` stripped)
          then Nothing
          else
            let afterImport = T.drop (T.length "import ") stripped
                -- Skip "qualified" and "safe" keywords.
                ws          = T.words afterImport
                rest        = dropWhile (`elem` (["qualified","safe"] :: [Text])) ws
            in  case rest of
                  []    -> Nothing
                  (_:remains) ->
                    -- `remains` might start with "(names)" or "as Alias (names)"
                    let joined = T.concat remains
                    in  if "(" `T.isPrefixOf` joined
                        then
                          let inner = T.drop 1 (T.takeWhile (/= ')') joined)
                          in  Just (map T.strip (T.splitOn "," inner))
                        else Nothing

-- ─────────────────────────────────────────────────────────────────────────────
-- Shared helpers
-- ─────────────────────────────────────────────────────────────────────────────

-- | Split a line at the first standalone binding @=@.
--
-- Skips @==@, @=\>@, @\<=@, @\>=@, @!=@, @-\>@ to avoid false positives.
-- Returns @Nothing@ for lines with no binding site (type signatures, data
-- declarations, guards, comments, etc.).
splitAtBinding :: Text -> Maybe (Text, Text)
splitAtBinding line = go [] (T.unpack line)
  where
    go acc ('=':'=':rest) = go (acc ++ "==") rest
    go acc ('=':'>':rest) = go (acc ++ "=>") rest
    go acc ('<':'=':rest) = go (acc ++ "<=") rest
    go acc ('>':'=':rest) = go (acc ++ ">=") rest
    go acc ('!':'=':rest) = go (acc ++ "!=") rest
    go acc ('-':'>':rest) = go (acc ++ "->") rest
    go acc ('=':rest)     = Just (T.pack acc, T.pack rest)
    go acc (c:rest)       = go (acc ++ [c]) rest
    go _   []             = Nothing

-- | True for a "simple variable" identifier: starts with a lowercase letter,
-- followed only by alphanumeric chars, underscores, or single quotes.
isSimpleVar :: Text -> Bool
isSimpleVar t
  | T.null t  = False
  | otherwise = isLower (T.head t)
                && T.all (\c -> isAlphaNum c || c == '_' || c == '\'') t

#else
-- ─────────────────────────────────────────────────────────────────────────────
-- Stub (with-exactprint flag absent)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Opaque stub — exactprint backend disabled at build time.
data HsModule = HsModule deriving (Show, Eq)

-- | A node in the Haskell source — stub.
data HsNode = HsNode
  { hnText :: !Text
  , hnKind :: !Text
  , hnLine :: !Int
  } deriving (Show, Eq)

-- | Stub mutation — no real transform.
data HsMutation = HsMutation
  { hmDescription :: !Text
  , hmTransform   :: Text -> Either Text Text
  }

-- | Alias for rule-set clarity.
type HsRewriteRule = HsMutation

-- | An ordered collection of rewrite rules.
type HsRuleSet = [HsRewriteRule]

-- | Always returns @'Left'@ when exactprint is not compiled in.
parseHsFile :: FilePath -> IO (Either Text HsModule)
parseHsFile _ = pure $ Left stubMsg

-- | Always returns @'Left'@ when exactprint is not compiled in.
parseHsText :: Text -> IO (Either Text HsModule)
parseHsText _ = pure $ Left stubMsg

-- | Returns empty text for the stub module.
printHsModule :: HsModule -> Text
printHsModule _ = ""

-- | Returns no nodes for the stub module.
hsNodes :: HsModule -> [HsNode]
hsNodes _ = []

-- | Get the display text of a node.
hsNodeText :: HsNode -> Text
hsNodeText = hnText

-- | Always returns @'Left'@ when exactprint is not compiled in.
applyHsMutation :: HsMutation -> HsModule -> Either Text HsModule
applyHsMutation _ _ = Left stubMsg

-- | Returns an empty list for the stub module.
collectHsMutations :: HsRuleSet -> HsModule -> [HsMutation]
collectHsMutations _ _ = []

-- | Empty rule set — exactprint disabled.
defaultHsRules :: HsRuleSet
defaultHsRules = []

stubMsg :: Text
stubMsg = "ghc-exactprint backend not enabled; build with -f+with-exactprint"

#endif

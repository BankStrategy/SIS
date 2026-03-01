{-# LANGUAGE BangPatterns #-}
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
    -- * Individual rules
  , unusedImportRule
  , addHaddockRule
  , addHaddockTransform
  , addTypeAnnotationRule
  , addTypeAnnotationTransform
  , limitedEtaReduceRule
  , limitedEtaReduceTransform
  , simplifyIfRule
  , simplifyIfTransform
    -- * Text-only module construction
  , mkTextModule
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isLower)
import Data.Maybe (listToMaybe, mapMaybe)

#ifdef WITH_EXACTPRINT
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EPP
import qualified GHC (ParsedSource)
import GHC.Paths (libdir)
import Control.Exception (try, SomeException)

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
  r <- try (EPP.parseModule libdir fp) :: IO (Either SomeException (EPP.ParseResult GHC.ParsedSource))
  pure $ case r of
    Left ex           -> Left (T.pack (show ex))
    Right (Left _)    -> Left ("parse error in " <> T.pack fp)
    Right (Right ps)  -> Right (HsModuleParsed ps)

-- | Parse Haskell source text directly (no temp file needed).
parseHsText :: Text -> IO (Either Text HsModule)
parseHsText src = do
  r <- try (EPP.parseModuleFromString libdir "<SIS-input>" (T.unpack src))
         :: IO (Either SomeException (EPP.ParseResult GHC.ParsedSource))
  pure $ case r of
    Left ex           -> Left (T.pack (show ex))
    Right (Left _)    -> Left "parse error in source text"
    Right (Right ps)  -> Right (HsModuleParsed ps)

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

-- | Construct a text-only 'HsModule' without parsing.
--
-- Use this for files that cannot be parsed by ghc-exactprint (e.g. CPP files).
-- The resulting module supports 'printHsModule' and 'applyHsMutation' via
-- text-level transforms, but 'hsNodes' will use text-level heuristics.
mkTextModule :: Text -> HsModule
mkTextModule = HsModuleMutated

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
-- Only includes unused-import removal.  The eta-reduce heuristic was removed
-- because text-level word-splitting cannot preserve multi-equation definitions,
-- guards, or operator sections — it produces incorrect code.
defaultHsRules :: HsRuleSet
defaultHsRules =
  [ unusedImportRule
  , addHaddockRule
  , addTypeAnnotationRule
  , limitedEtaReduceRule
  , simplifyIfRule
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
  let ls          = T.lines src
      -- Identify import lines with explicit lists where none of the
      -- listed names appears anywhere else in the source.
      toRemove    = filter (isUnusedExplicitImport ls) ls
  in  if null toRemove
      then Left "no unused explicit imports detected"
      else Right $ T.unlines $ filter (`notElem` toRemove) ls
  where
    isImportLine :: Text -> Bool
    isImportLine l = "import " `T.isPrefixOf` T.stripStart l

    -- | True if this line is an explicit import (@import M (names)@) AND
    -- none of the listed bare names appear anywhere in the source (excluding
    -- the import line itself).  Uses whole-word matching to avoid false
    -- positives like "Map" matching "MapStrict".
    isUnusedExplicitImport :: [Text] -> Text -> Bool
    isUnusedExplicitImport allLines line =
      case extractExplicitNames line of
        Nothing    -> False  -- not an explicit import
        Just []    -> False  -- empty list — keep for side effects
        Just names ->
          -- Check each name against the entire source *excluding* this
          -- specific import line.  This catches re-exports in the module
          -- header, qualified usage in other imports, etc.
          let rest = T.unlines (filter (/= line) allLines)
          in  all (\n ->
                let base = T.strip (T.takeWhile (/= '(') n)
                in  T.null base || countWholeWord base rest == 0) names

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

-- | True if 'c' is a Haskell identifier character.
isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- | Count whole-word occurrences of @needle@ in @haystack@.
countWholeWord :: Text -> Text -> Int
countWholeWord needle haystack = go 0 haystack
  where
    nLen = T.length needle
    go !n hay
      | T.null hay = n
      | otherwise  =
          case T.breakOn needle hay of
            (_, rest) | T.null rest -> n
            (before, rest) ->
              let charBefore = if T.null before then Nothing else Just (T.last before)
                  afterNeedle = T.drop nLen rest
                  charAfter  = if T.null afterNeedle then Nothing else Just (T.head afterNeedle)
                  bounded    = not (maybe False isIdentChar charBefore)
                            && not (maybe False isIdentChar charAfter)
              in  go (if bounded then n + 1 else n) (T.drop 1 rest)

-- | Replace the first whole-word occurrence of @needle@ with @replacement@.
replaceWholeWord :: Text -> Text -> Text -> Text
replaceWholeWord needle replacement haystack = go haystack
  where
    nLen = T.length needle
    go hay
      | T.null hay = hay
      | otherwise  =
          case T.breakOn needle hay of
            (_, rest) | T.null rest -> hay
            (before, rest) ->
              let charBefore = if T.null before then Nothing else Just (T.last before)
                  afterNeedle = T.drop nLen rest
                  charAfter  = if T.null afterNeedle then Nothing else Just (T.head afterNeedle)
                  bounded    = not (maybe False isIdentChar charBefore)
                            && not (maybe False isIdentChar charAfter)
              in  if bounded
                  then before <> replacement <> afterNeedle
                  else before <> T.take 1 rest <> go (T.drop 1 rest)

-- ─────────────────────────────────────────────────────────────────────────────
-- Rule: inline single-use let
-- ─────────────────────────────────────────────────────────────────────────────

-- | Rule: inline a single-use @let x = expr in body@ on a single line.
--
-- Finds the first occurrence of @let x = expr in body@ where @x@ appears
-- exactly once in @body@ (as a whole word), and substitutes @(expr)@ for @x@.
addHaddockRule :: HsMutation
addHaddockRule = HsMutation
  { hmDescription = "inline-single-use-let"
  , hmTransform   = addHaddockTransform
  }

addHaddockTransform :: Text -> Either Text Text
addHaddockTransform src =
  let ls      = zip [0 :: Int ..] (T.lines src)
      results = mapMaybe tryInlineLet ls
  in  case results of
        []             -> Left "no single-use let bindings found"
        (i, newLine):_ ->
          let original = T.lines src
              patched  = take i original ++ [newLine] ++ drop (i + 1) original
          in  Right (T.unlines patched)
  where
    tryInlineLet :: (Int, Text) -> Maybe (Int, Text)
    tryInlineLet (idx, line) =
      let stripped = T.strip line
          indent   = T.takeWhile (== ' ') line
      in  case T.breakOn "let " stripped of
            (before, rest)
              | T.null rest -> Nothing
              | otherwise   ->
                let afterLet = T.drop 4 rest  -- drop "let "
                in  case T.breakOn " = " afterLet of
                      (_, rhs) | T.null rhs -> Nothing
                      (var, rhs') ->
                        let varT     = T.strip var
                            afterEq  = T.drop 3 rhs'  -- drop " = "
                        in  case T.breakOn " in " afterEq of
                              (_, bodyPart) | T.null bodyPart -> Nothing
                              (expr, bodyPart') ->
                                let bodyT = T.drop 4 bodyPart'  -- drop " in "
                                in  if isSimpleVar varT
                                       && countWholeWord varT bodyT == 1
                                    then
                                      let replaced = replaceWholeWord varT ("(" <> expr <> ")") bodyT
                                      in  Just (idx, indent <> before <> replaced)
                                    else Nothing

-- ─────────────────────────────────────────────────────────────────────────────
-- Rule: remove dead let
-- ─────────────────────────────────────────────────────────────────────────────

-- | Rule: remove a dead @let x = expr in body@ where @x@ is unused in @body@.
addTypeAnnotationRule :: HsMutation
addTypeAnnotationRule = HsMutation
  { hmDescription = "remove-dead-let"
  , hmTransform   = addTypeAnnotationTransform
  }

addTypeAnnotationTransform :: Text -> Either Text Text
addTypeAnnotationTransform src =
  let ls      = zip [0 :: Int ..] (T.lines src)
      results = mapMaybe tryDeadLet ls
  in  case results of
        []             -> Left "no dead let bindings found"
        (i, newLine):_ ->
          let original = T.lines src
              patched  = take i original ++ [newLine] ++ drop (i + 1) original
          in  Right (T.unlines patched)
  where
    tryDeadLet :: (Int, Text) -> Maybe (Int, Text)
    tryDeadLet (idx, line) =
      let stripped = T.strip line
          indent   = T.takeWhile (== ' ') line
      in  case T.breakOn "let " stripped of
            (before, rest)
              | T.null rest -> Nothing
              | otherwise   ->
                let afterLet = T.drop 4 rest
                in  case T.breakOn " = " afterLet of
                      (_, rhs) | T.null rhs -> Nothing
                      (var, rhs') ->
                        let varT    = T.strip var
                            afterEq = T.drop 3 rhs'
                        in  case T.breakOn " in " afterEq of
                              (_, bodyPart) | T.null bodyPart -> Nothing
                              (_expr, bodyPart') ->
                                let bodyT = T.drop 4 bodyPart'
                                in  if isSimpleVar varT
                                       && countWholeWord varT bodyT == 0
                                    then Just (idx, indent <> before <> bodyT)
                                    else Nothing

-- ─────────────────────────────────────────────────────────────────────────────
-- Rule: limited eta-reduce (single-equation only)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Safer version of eta-reduction that only fires on single-equation,
-- single-argument, single-line definitions of the form @f x = g x@.
limitedEtaReduceRule :: HsMutation
limitedEtaReduceRule = HsMutation
  { hmDescription = "eta-reduce-single-eq"
  , hmTransform   = limitedEtaReduceTransform
  }

limitedEtaReduceTransform :: Text -> Either Text Text
limitedEtaReduceTransform src =
  let ls   = T.lines src
      mIdx = findEtaCandidate ls
  in  case mIdx of
        Nothing        -> Left "no eta-reducible single-equation bindings found"
        Just (i, reduced) ->
          Right (T.unlines (take i ls ++ [reduced] ++ drop (i+1) ls))
  where
    -- Find a line of the form "f x = g x" where the line fits the pattern:
    -- exactly "name arg = expr arg"
    findEtaCandidate :: [Text] -> Maybe (Int, Text)
    findEtaCandidate lns =
      listToMaybe [ (i, reduced)
                  | (i, l) <- zip [0..] lns
                  , Just reduced <- [tryEtaReduce l]
                  ]

    tryEtaReduce :: Text -> Maybe Text
    tryEtaReduce line =
      -- Pattern: "fname arg = expr arg"  where arg is the last word on both sides
      let ws = T.words line
      in  case ws of
            (fname : args)
              | not (T.null fname)
              , isLower (T.head fname)
              , length args >= 3                  -- at least: arg = expr arg
              , last ws == last (init ws)         -- last arg on LHS == last arg on RHS
              , "=" `elem` ws                     -- has = sign
              ->
                  let eqIdx = length (takeWhile (/= "=") ws)
                      lhsArgs = take (eqIdx - 1) (tail ws)  -- args before =
                      lastArg = last lhsArgs
                      rhs     = drop (eqIdx + 1) ws
                  in  if not (null lhsArgs)
                       && not (null rhs)
                       && last rhs == lastArg
                       && length lhsArgs == 1  -- single argument only
                       then Just (T.unwords (fname : init lhsArgs ++ ["="] ++ init rhs))
                       else Nothing
            _ -> Nothing

-- ─────────────────────────────────────────────────────────────────────────────
-- Rule: simplify constant if
-- ─────────────────────────────────────────────────────────────────────────────

-- | Simplify @if True then X else Y@ to @X@ and @if False then X else Y@ to @Y@.
simplifyIfRule :: HsMutation
simplifyIfRule = HsMutation
  { hmDescription = "simplify-constant-if"
  , hmTransform   = simplifyIfTransform
  }

simplifyIfTransform :: Text -> Either Text Text
simplifyIfTransform src =
  let ls      = zip [0 :: Int ..] (T.lines src)
      results = mapMaybe trySimplifyIf ls
  in  case results of
        []             -> Left "no constant-condition if expressions found"
        (i, newLine):_ ->
          let original = T.lines src
              patched  = take i original ++ [newLine] ++ drop (i + 1) original
          in  Right (T.unlines patched)
  where
    trySimplifyIf :: (Int, Text) -> Maybe (Int, Text)
    trySimplifyIf (idx, line) =
      let indent = T.takeWhile (== ' ') line
      in  case tryReplace "if True then " " else " line of
            Just thenBranch -> Just (idx, indent <> T.strip thenBranch)
            Nothing ->
              case tryReplace "if False then " " else " line of
                Just elseBranch -> Just (idx, indent <> T.strip elseBranch)
                Nothing         -> Nothing

    -- For "if True then X else Y", extract X.
    -- For "if False then X else Y", extract Y.
    tryReplace :: Text -> Text -> Text -> Maybe Text
    tryReplace ifPat elsePat line =
      let stripped = T.strip line
      in  case T.breakOn ifPat stripped of
            (_, rest) | T.null rest -> Nothing
            (before, rest) ->
              let afterIf = T.drop (T.length ifPat) rest
              in  case T.breakOn elsePat afterIf of
                    (_, elseRest) | T.null elseRest -> Nothing
                    (thenBranch, elseRest) ->
                      let elseBranch = T.drop (T.length elsePat) elseRest
                      in  if ifPat == "if True then "
                          then Just (before <> thenBranch)
                          else Just (before <> elseBranch)

#else
-- ─────────────────────────────────────────────────────────────────────────────
-- Stub (with-exactprint flag absent)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Opaque stub — exactprint backend disabled at build time.
--
-- 'HsModuleText' carries raw source text for Oracle text-level mutations.
data HsModule = HsModule | HsModuleText !Text deriving (Show, Eq)

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

-- | Returns empty text for the stub module, or the stored text for 'HsModuleText'.
printHsModule :: HsModule -> Text
printHsModule HsModule        = ""
printHsModule (HsModuleText t) = t

-- | Returns no nodes for the stub module.
hsNodes :: HsModule -> [HsNode]
hsNodes _ = []

-- | Get the display text of a node.
hsNodeText :: HsNode -> Text
hsNodeText = hnText

-- | Applies a mutation via text transform for 'HsModuleText'; always fails for 'HsModule'.
applyHsMutation :: HsMutation -> HsModule -> Either Text HsModule
applyHsMutation _ HsModule = Left stubMsg
applyHsMutation mut (HsModuleText src) =
  case hmTransform mut src of
    Left err -> Left err
    Right t  -> Right (HsModuleText t)

-- | Construct a text-only 'HsModule' without parsing.
mkTextModule :: Text -> HsModule
mkTextModule = HsModuleText

-- | Returns an empty list for the stub module.
collectHsMutations :: HsRuleSet -> HsModule -> [HsMutation]
collectHsMutations _ _ = []

-- | Empty rule set — exactprint disabled.
defaultHsRules :: HsRuleSet
defaultHsRules = []

-- | Stub — always Left.
unusedImportRule :: HsMutation
unusedImportRule = HsMutation "remove unused explicit imports" (\_ -> Left stubMsg)

-- | Stub — always Left.
addHaddockRule :: HsMutation
addHaddockRule = HsMutation "inline-single-use-let" addHaddockTransform

-- | Stub — always Left.
addHaddockTransform :: Text -> Either Text Text
addHaddockTransform _ = Left stubMsg

-- | Stub — always Left.
addTypeAnnotationRule :: HsMutation
addTypeAnnotationRule = HsMutation "remove-dead-let" addTypeAnnotationTransform

-- | Stub — always Left.
addTypeAnnotationTransform :: Text -> Either Text Text
addTypeAnnotationTransform _ = Left stubMsg

-- | Stub — always Left.
limitedEtaReduceRule :: HsMutation
limitedEtaReduceRule = HsMutation "eta-reduce-single-eq" limitedEtaReduceTransform

-- | Stub — always Left.
limitedEtaReduceTransform :: Text -> Either Text Text
limitedEtaReduceTransform _ = Left stubMsg

-- | Stub — always Left.
simplifyIfRule :: HsMutation
simplifyIfRule = HsMutation "simplify-constant-if" simplifyIfTransform

-- | Stub — always Left.
simplifyIfTransform :: Text -> Either Text Text
simplifyIfTransform _ = Left stubMsg

stubMsg :: Text
stubMsg = "ghc-exactprint backend not enabled; build with -f+with-exactprint"

#endif

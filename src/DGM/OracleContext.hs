-- | DGM.OracleContext — GHC-guided Oracle prompts (SI-du9).
--
-- Runs @cabal build -Wall@ to collect GHC warnings, parses them into
-- structured 'GhcWarning' values, and builds enriched prompts that tell
-- the Oracle /exactly/ which removals are verified safe.
--
-- The Oracle's job shifts from "guess what's unused" to "pick ONE of these
-- GHC-confirmed safe removals and format it as a diff."
module DGM.OracleContext
  ( -- * Warning types
    GhcWarning(..)
  , WarningKind(..)
    -- * Module context
  , ModuleContext(..)
    -- * Warning collection
  , collectGhcWarnings
    -- * Parsing (pure, testable)
  , parseGhcWarnings
    -- * Context building (pure)
  , buildModuleContext
    -- * Enriched prompt (pure)
  , buildEnrichedPrompt
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, tryPutMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, catch, evaluate)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.IO (hGetContents, hClose)
import System.Process (createProcess, proc, std_out, std_err, StdStream(..), waitForProcess, terminateProcess, ProcessHandle)

import DGM.ModGraph (ModuleGraph(..), extractExports)
import DGM.Oracle (MutationContext)
import qualified Data.Map.Strict as Map

-- ─────────────────────────────────────────────────────────────────────────────
-- Types
-- ─────────────────────────────────────────────────────────────────────────────

-- | A single GHC warning parsed from @-Wall@ output.
data GhcWarning = GhcWarning
  { gwFile    :: FilePath     -- ^ e.g. @"src/DGM/Foo.hs"@
  , gwLine    :: Int          -- ^ line number
  , gwKind    :: WarningKind
  , gwMessage :: Text         -- ^ raw GHC warning text
  } deriving (Show, Eq)

-- | Classified warning kind.
data WarningKind
  = UnusedImport Text         -- ^ the imported name/module
  | UnusedBind Text           -- ^ the unused binding name
  | MissingSignature Text     -- ^ the function name
  | RedundantConstraint Text  -- ^ the constraint
  | OtherWarning              -- ^ anything else
  deriving (Show, Eq)

-- | Context about a single module for enriched prompting.
data ModuleContext = ModuleContext
  { mcFilePath    :: FilePath
  , mcSource      :: Text
  , mcWarnings    :: [GhcWarning]       -- ^ GHC warnings for this file
  , mcExports     :: [Text]             -- ^ exported names (DO NOT TOUCH)
  , mcReverseDeps :: [Text]             -- ^ modules that import this one
  } deriving (Show, Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- Warning collection
-- ─────────────────────────────────────────────────────────────────────────────

-- | Run @cabal build -Wall@ and parse the resulting warnings.
--
-- 60-second timeout; returns @[]@ on timeout or failure.
collectGhcWarnings :: IO [GhcWarning]
collectGhcWarnings = do
  (_, Just _hOut, Just hErr, ph) <-
    createProcess (proc cabalBin
      [ "build"
      , "--with-compiler=" ++ ghcBin
      , "--ghc-options=-Wall"
      ])
      { std_out = CreatePipe
      , std_err = CreatePipe
      }
  -- Slurp stderr (GHC warnings go there).
  errMVar <- newEmptyMVar
  _ <- forkIO (slurp hErr >>= putMVar errMVar)
  mec <- withTimeout 60 ph
  errText <- takeMVar errMVar
  case mec of
    Nothing -> return []  -- timeout
    Just (ExitFailure _) ->
      -- Build failure is expected when there ARE warnings;
      -- parse what we got.
      return (parseGhcWarnings (T.pack errText))
    Just ExitSuccess ->
      return (parseGhcWarnings (T.pack errText))
  where
    slurp h = do
      s <- hGetContents h
      _ <- evaluate (length s)
      hClose h
      return s

-- ─────────────────────────────────────────────────────────────────────────────
-- Pure warning parser
-- ─────────────────────────────────────────────────────────────────────────────

-- | Parse GHC warning output into structured 'GhcWarning' values.
--
-- Recognises the standard GHC format:
--
-- > src/DGM/Foo.hs:42:1: warning: [-Wunused-imports]
-- >     The import of 'Data.List' is redundant
parseGhcWarnings :: Text -> [GhcWarning]
parseGhcWarnings input =
  let ls = T.lines input
  in  concatMap (parseWarningLine ls) (zip [0..] ls)
  where
    parseWarningLine :: [Text] -> (Int, Text) -> [GhcWarning]
    parseWarningLine allLines (idx, line) =
      case parseLocationLine line of
        Nothing -> []
        Just (fp, ln, warnFlag) ->
          let -- Collect continuation lines (indented, before next warning header)
              contLines = takeWhile isContLine (drop (idx + 1) allLines)
              detail    = T.strip (T.unlines contLines)
              kind      = classifyWarning warnFlag detail
          in  [GhcWarning fp ln kind (T.strip line <> " " <> detail)]

    isContLine l = T.isPrefixOf "    " l || T.null (T.strip l)

    -- Parse "src/DGM/Foo.hs:42:1: warning: [-Wunused-imports]"
    parseLocationLine :: Text -> Maybe (FilePath, Int, Text)
    parseLocationLine l =
      let parts = T.splitOn ":" l
      in case parts of
           (fp : lineNum : _ : rest)
             | T.all isDigit (T.strip lineNum)
             , not (T.null (T.strip lineNum))
             , "warning" `T.isInfixOf` T.unwords rest
             ->
               let ln = read (T.unpack (T.strip lineNum))
                   warnFlag = extractWarnFlag (T.unwords rest)
               in  Just (T.unpack (T.strip fp), ln, warnFlag)
           _ -> Nothing

    extractWarnFlag :: Text -> Text
    extractWarnFlag t =
      case T.breakOn "[-W" t of
        (_, rest) | not (T.null rest) ->
          -- rest = "[-Wunused-imports]...", drop "[" then take until "]"
          let inner = T.takeWhile (/= ']') (T.drop 1 rest)
          in  T.strip inner
        _ -> ""

    classifyWarning :: Text -> Text -> WarningKind
    classifyWarning flag detail
      | "-Wunused-imports" `T.isInfixOf` flag =
          UnusedImport (extractQuoted detail)
      | "-Wunused-top-binds" `T.isInfixOf` flag =
          UnusedBind (extractQuoted detail)
      | "-Wmissing-signatures" `T.isInfixOf` flag =
          MissingSignature (extractQuoted detail)
      | "-Wredundant-constraints" `T.isInfixOf` flag =
          RedundantConstraint (extractQuoted detail)
      | otherwise = OtherWarning

    -- Extract the first 'quoted' or `backticked` name from warning detail.
    extractQuoted :: Text -> Text
    extractQuoted t =
      case T.breakOn "\8216" t of  -- left single quotation mark '
        (_, rest) | not (T.null rest) ->
          T.takeWhile (/= '\8217') (T.drop 1 rest)  -- right single quotation mark '
        _ -> case T.breakOn "'" t of
               (_, rest2) | not (T.null rest2) ->
                 T.takeWhile (/= '\'') (T.drop 1 rest2)
               _ -> ""

-- ─────────────────────────────────────────────────────────────────────────────
-- Context building
-- ─────────────────────────────────────────────────────────────────────────────

-- | Build a 'ModuleContext' for one file, filtering warnings and looking up
-- exports and reverse dependencies from the 'ModuleGraph'.
buildModuleContext
  :: ModuleGraph -> [GhcWarning] -> FilePath -> Text -> ModuleContext
buildModuleContext mg allWarnings fp src =
  let fileWarnings = filter (\w -> gwFile w == fp) allWarnings
      exports = extractExports src
      -- Look up reverse deps: find module name for this file path,
      -- then look up who imports it.
      mModName = case [ nm | (nm, path) <- Map.toList (mgModules mg)
                           , path == fp ] of
                   (n:_) -> Just n
                   []    -> Nothing
      revDeps = case mModName of
                  Nothing -> []
                  Just nm -> maybe [] id (Map.lookup nm (mgUsedBy mg))
  in ModuleContext
       { mcFilePath    = fp
       , mcSource      = src
       , mcWarnings    = fileWarnings
       , mcExports     = exports
       , mcReverseDeps = revDeps
       }

-- ─────────────────────────────────────────────────────────────────────────────
-- Enriched prompt
-- ─────────────────────────────────────────────────────────────────────────────

-- | Build an enriched prompt for the Oracle with verified safe operations.
--
-- When warnings are present, the prompt has three sections:
-- VERIFIED SAFE REMOVALS, DO NOT TOUCH, and INSTRUCTIONS.
--
-- When no warnings exist for the file, returns 'Nothing' so the caller
-- can fall back to the standard prompt.
buildEnrichedPrompt :: ModuleContext -> Maybe MutationContext -> Maybe Text
buildEnrichedPrompt ctx _mCtx
  | null (mcWarnings ctx) = Nothing
  | otherwise = Just $ T.unlines $
      [ "## VERIFIED SAFE REMOVALS (GHC -Wall confirms these are unused)"
      ] ++ map formatWarning (mcWarnings ctx) ++
      [ ""
      , "## DO NOT TOUCH (these names are exported or used by other modules)"
      ] ++ exportLines ++ revDepLines ++
      [ ""
      , "## SOURCE"
      , "---"
      ] ++ zipWith (\i l -> T.pack (show i) <> " " <> l)
                   [1::Int ..] (T.lines (mcSource ctx)) ++
      [ "---"
      , ""
      , "## INSTRUCTIONS"
      , "Pick exactly ONE item from VERIFIED SAFE REMOVALS and produce a unified diff"
      , "that removes it. Do not modify anything else. Do not touch exported names."
      , "Output ONLY the diff (--- / +++ format). No explanation, no fences."
      ]
  where
    formatWarning w =
      "- Line " <> T.pack (show (gwLine w)) <> ": " <> describeKind (gwKind w)

    describeKind (UnusedImport name)        = "unused import '" <> name <> "' [-Wunused-imports]"
    describeKind (UnusedBind name)          = "unused binding '" <> name <> "' [-Wunused-top-binds]"
    describeKind (MissingSignature name)    = "missing signature for '" <> name <> "' [-Wmissing-signatures]"
    describeKind (RedundantConstraint name) = "redundant constraint '" <> name <> "' [-Wredundant-constraints]"
    describeKind OtherWarning               = "other warning"

    exportLines =
      if null (mcExports ctx)
        then ["- (no explicit export list)"]
        else ["- Exported: " <> T.intercalate ", " (mcExports ctx)]

    revDepLines =
      if null (mcReverseDeps ctx)
        then []
        else ["- Used by: " <> T.intercalate ", " (mcReverseDeps ctx)]

-- ─────────────────────────────────────────────────────────────────────────────
-- Configuration
-- ─────────────────────────────────────────────────────────────────────────────

cabalBin :: FilePath
cabalBin = "/Users/raz/.ghcup/bin/cabal"

ghcBin :: FilePath
ghcBin = "/Users/raz/.ghcup/bin/ghc-9.6"

-- | Wait for @ph@ to exit, returning 'Nothing' if it exceeds @secs@ seconds.
withTimeout :: Int -> ProcessHandle -> IO (Maybe ExitCode)
withTimeout secs ph = do
  resultMVar <- newEmptyMVar
  _ <- forkIO $ do
    ec <- waitForProcess ph `catch` \e ->
            let _ = e :: SomeException
            in  terminateProcess ph >> return (ExitFailure 1)
    _ <- tryPutMVar resultMVar (Just ec)
    return ()
  _ <- forkIO $ do
    threadDelay (secs * 1000000)
    terminateProcess ph
    _ <- tryPutMVar resultMVar Nothing
    return ()
  takeMVar resultMVar

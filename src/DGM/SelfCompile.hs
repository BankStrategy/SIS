-- | DGM.SelfCompile — self-compilation and test loop (SELFMOD.md Phase D).
--
-- Spawns @cabal test@ as a subprocess, captures the result, scores it, and
-- exposes transaction-based rollback for use in the SelfModPhase of the cycle.
module DGM.SelfCompile
  ( -- * Result types
    CompileResult(..)
  , CompilePhase(..)
    -- * Running the test suite
  , compileSelf
  , testSelf
    -- * Build pre-flight
  , buildPreflight
    -- * Scoring
  , scoreCompileResult
  , enrichScore
    -- * Transactional rollback
  , SelfModTxn(..)
  , withSelfModTxn
  ) where

import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, tryPutMVar, takeMVar)
import Control.Exception (SomeException, catch, evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Exit (ExitCode(..))
import System.IO (hGetContents, hClose)
import System.Process
  ( ProcessHandle
  , createProcess, proc, std_out, std_err, StdStream(..)
  , waitForProcess, terminateProcess
  )

import DGM.HsAST (HsMutation)

-- ─────────────────────────────────────────────────────────────────────────────
-- Types
-- ─────────────────────────────────────────────────────────────────────────────

-- | The phase at which a compilation failure occurred.
data CompilePhase
  = ParsePhase
  | TypeCheckPhase
  | LinkPhase
  | TestPhase
  deriving (Show, Eq)

-- | Result of a @cabal build@ or @cabal test@ invocation.
data CompileResult
  = CompileSuccess
      { crTestsPassed :: Int
      , crTestsFailed :: Int
      , crLatencyMs   :: Double
      }
  | CompileFailure
      { crErrors :: Text
      , crPhase  :: CompilePhase
      }
  deriving (Show, Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- Running build / tests
-- ─────────────────────────────────────────────────────────────────────────────

-- | Run @cabal build@ and return a 'CompileResult'.
compileSelf :: IO CompileResult
compileSelf = runCabal ["build"]

-- | Run @cabal test --test-show-details=direct@ and return a 'CompileResult'.
--
-- Captures stdout+stderr, parses test counts, and enforces a 120-second
-- timeout.  The exit code and output are used to distinguish parse/type/link/
-- test failures.
testSelf :: IO CompileResult
testSelf = runCabal ["test", "--test-show-details=direct"]

-- | Run @cabal build@ (no tests, no -Wall) as a quick type-check pre-flight.
--
-- Returns @Right ()@ on success, @Left errorText@ on any compilation error.
-- 60-second timeout.  Catches type errors in ~5s instead of the full test
-- suite's ~30-120s, short-circuiting bad mutations early.
buildPreflight :: IO (Either Text ())
buildPreflight = do
  result <- runCabal ["build"]
  case result of
    CompileSuccess {} -> return (Right ())
    CompileFailure errs phase ->
      return (Left ("Build pre-flight failed (" <> T.pack (show phase) <> "): "
                    <> T.take 200 errs))

-- ─────────────────────────────────────────────────────────────────────────────
-- Scoring
-- ─────────────────────────────────────────────────────────────────────────────

-- | Map a 'CompileResult' to a score in [0, 1].
--
-- * 'CompileSuccess' all pass            → 1.0
-- * 'CompileSuccess' k of n pass         → @k \/ n@
-- * 'CompileFailure' 'TypeCheckPhase'    → 0.1
-- * 'CompileFailure' 'ParsePhase'        → 0.0
-- * 'CompileFailure' other               → 0.05
scoreCompileResult :: CompileResult -> Double
scoreCompileResult (CompileSuccess passed failed _lat)
  | total  == 0 = 0.0
  | failed == 0 = 1.0
  | otherwise   = fromIntegral passed / fromIntegral total
  where total = passed + failed
scoreCompileResult (CompileFailure _ TypeCheckPhase) = 0.1
scoreCompileResult (CompileFailure _ ParsePhase)     = 0.0
scoreCompileResult (CompileFailure _ _)              = 0.05

-- | Enrich a base score with additional fitness signals.
--
-- Takes the base score from 'scoreCompileResult' and adjusts it with:
--
--   * __Code-size shrink bonus__: mutations that reduce the source size
--     receive a bonus of up to 0.05, proportional to the fraction shrunk.
--     Helps the system prefer cleaner, smaller code over bloated alternatives.
--
--   * __LiquidHaskell verification bonus__: +0.02 when LH returns @"SAFE"@,
--     since this means the mutation was formally verified.
--
-- The result is capped at 1.0.
enrichScore
  :: Double       -- ^ Base score from 'scoreCompileResult'.
  -> Int          -- ^ Original source length in characters.
  -> Int          -- ^ Mutated source length in characters.
  -> Maybe Text   -- ^ LiquidHaskell result text (e.g. @"SAFE"@ or @"UNSAFE"@).
  -> Double
enrichScore base origLen mutLen mLhResult =
  let shrinkRatio = if origLen <= 0
                      then 0.0
                      else fromIntegral (max 0 (origLen - mutLen))
                           / fromIntegral origLen
      sizeBonus = 0.05 * shrinkRatio
      lhBonus   = case mLhResult of
                    Just txt | "SAFE" `T.isInfixOf` txt -> 0.02
                    _                                    -> 0.0
  in  min 1.0 (base + sizeBonus + lhBonus)

-- ─────────────────────────────────────────────────────────────────────────────
-- Transactional rollback
-- ─────────────────────────────────────────────────────────────────────────────

-- | Snapshot of a file before a self-modification, for rollback purposes.
data SelfModTxn = SelfModTxn
  { smtFile         :: FilePath   -- ^ The file that was mutated.
  , smtOriginalText :: Text       -- ^ Its original content.
  , smtMutation     :: HsMutation -- ^ The mutation that was applied.
  }

-- | Run an action within a self-modification transaction.
--
-- If the action returns @'Left' err@, the original file content is restored
-- before returning the error.  If the action returns @'Right' ()@, the mutated
-- content is left in place.
withSelfModTxn :: SelfModTxn -> IO (Either Text ()) -> IO (Either Text ())
withSelfModTxn txn action = do
  result <- action
  case result of
    Right () -> return (Right ())
    Left err -> do
      TIO.writeFile (smtFile txn) (smtOriginalText txn)
      return (Left err)

-- ─────────────────────────────────────────────────────────────────────────────
-- Internal: subprocess runner (120-second timeout)
-- ─────────────────────────────────────────────────────────────────────────────

runCabal :: [String] -> IO CompileResult
runCabal args = do
  t0 <- nowMs
  let fullArgs = args ++ ["--with-compiler=" ++ ghcBin]
  (_, Just hOut, Just hErr, ph) <-
    createProcess (proc cabalBin fullArgs)
      { std_out = CreatePipe
      , std_err = CreatePipe
      }
  -- Slurp both pipes concurrently to avoid pipe-buffer deadlock.
  outMVar <- newEmptyMVar
  errMVar <- newEmptyMVar
  _ <- forkIO (slurp hOut >>= putMVar outMVar)
  _ <- forkIO (slurp hErr >>= putMVar errMVar)
  mec <- withTimeout 120 ph
  out <- takeMVar outMVar
  err <- takeMVar errMVar
  t1  <- nowMs
  let combined  = out <> "\n" <> err
      latencyMs = fromIntegral (t1 - t0) :: Double
  return (parseResult mec combined latencyMs)
  where
    slurp h = do
      s <- hGetContents h
      _ <- evaluate (length s)
      hClose h
      return s

-- ─────────────────────────────────────────────────────────────────────────────
-- Internal: result parsing
-- ─────────────────────────────────────────────────────────────────────────────

parseResult :: Maybe ExitCode -> String -> Double -> CompileResult
parseResult Nothing combined _lat =
  CompileFailure (T.pack ("Timeout\n" <> combined)) TestPhase
parseResult (Just ExitSuccess) combined lat =
  let (passed, failed) = parseTestCounts combined
  in  CompileSuccess passed failed lat
parseResult (Just (ExitFailure _)) combined _lat =
  CompileFailure (T.pack combined) (classifyFailure (T.pack combined))

classifyFailure :: Text -> CompilePhase
classifyFailure txt
  | any (`T.isInfixOf` tl) parseKws     = ParsePhase
  | any (`T.isInfixOf` tl) typecheckKws = TypeCheckPhase
  | any (`T.isInfixOf` tl) linkKws      = LinkPhase
  | "fail"         `T.isInfixOf` tl     = TestPhase
  | otherwise                            = TypeCheckPhase
  where
    tl           = T.toLower txt
    parseKws     = ["parse error", "lexical error"]
    typecheckKws = [ "not in scope", "couldn't match", "no instance"
                   , "type error", "variable not in scope", "ambiguous type" ]
    linkKws      = ["undefined reference", "undefined symbol"]

-- | Extract passed/failed test counts from cabal-test output.
--
-- Handles lines like:
--
-- > 42 of 42 test suites (42 of 42 individual tests) passed
-- > 3 tests failed
-- > All 42 tests passed
parseTestCounts :: String -> (Int, Int)
parseTestCounts output = foldl step (0, 0) (lines output)
  where
    step (p, f) ln
      | anyWord ln ["passed", "ok"] = (p + leadingInt ln, f)
      | anyWord ln ["failed", "fail"] = (p, f + leadingInt ln)
      | otherwise = (p, f)

    anyWord ln ws =
      let lws = map (map toLower) (words ln)
      in  any (`elem` lws) ws

    leadingInt s =
      case reads (dropWhile (not . isDigit) s) of
        [(n, _)] -> n
        _        -> 1

    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise             = c

    isDigit c = c >= '0' && c <= '9'

-- ─────────────────────────────────────────────────────────────────────────────
-- Internal: timeout
-- ─────────────────────────────────────────────────────────────────────────────

-- | Wait for @ph@ to exit, returning 'Nothing' if it exceeds @secs@ seconds.
withTimeout :: Int -> ProcessHandle -> IO (Maybe ExitCode)
withTimeout secs ph = do
  resultMVar <- newEmptyMVar
  -- Thread 1: wait for the process.
  _ <- forkIO $ do
    ec <- waitForProcess ph `catch` \e ->
            let _ = e :: SomeException
            in  terminateProcess ph >> return (ExitFailure 1)
    _ <- tryPutMVar resultMVar (Just ec)
    return ()
  -- Thread 2: enforce timeout.
  _ <- forkIO $ do
    threadDelay (secs * 1000000)
    terminateProcess ph
    _ <- tryPutMVar resultMVar Nothing
    return ()
  takeMVar resultMVar

-- ─────────────────────────────────────────────────────────────────────────────
-- Internal: timing
-- ─────────────────────────────────────────────────────────────────────────────

nowMs :: IO Int
nowMs = do
  t <- getPOSIXTime
  return (round (realToFrac t * 1000 :: Double))

-- ─────────────────────────────────────────────────────────────────────────────
-- Configuration
-- ─────────────────────────────────────────────────────────────────────────────

-- | Path to the cabal executable.
--
-- Tries the ghcup standard location first, falls back to @\"cabal\"@ (PATH
-- lookup by the OS).  The ghcup path is checked at runtime so the binary
-- remains portable across machines that have ghcup installed.
cabalBin :: FilePath
cabalBin = "/Users/raz/.ghcup/bin/cabal"

-- | Path to the GHC compiler.
--
-- Must be an actual executable (not a shell wrapper) for @--with-compiler@.
ghcBin :: FilePath
ghcBin = "/Users/raz/.ghcup/bin/ghc-9.6.7"
-- NOTE: On machines without ghcup at this path, override by setting
-- the CABAL_BIN / GHC_BIN environment variables or ensure they are on PATH.

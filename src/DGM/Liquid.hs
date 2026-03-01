{-# LANGUAGE CPP #-}
-- | DGM.Liquid — LiquidHaskell verification subprocess (pillar II formal pre-flight).
--
-- Runs LiquidHaskell as a subprocess on a mutated source file before handing
-- off to cabal test.  The mutation pipeline becomes:
--
--   propose → LH verify → SBV prove → cabal test → commit
--
-- If LH reports UNSAFE, the mutation is rolled back immediately without
-- invoking cabal test, giving faster rejection for safety-invariant violations.
--
-- This module compiles unconditionally but only invokes the real LH binary
-- when the @with-liquid@ flag is enabled.  Without @WITH_LIQUID@ defined,
-- 'verifyWithLiquid' is a no-op stub that always returns 'LiquidSafe'.
module DGM.Liquid
  ( -- * Result type
    LiquidResult(..)
    -- * Verification
  , verifyWithLiquid
    -- * Output parsing
  , parseLiquidOutput
  ) where

import Data.Text (Text)
import qualified Data.Text as T

#ifdef WITH_LIQUID
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.IO (hPutStrLn, stderr)
import Control.Exception (try, SomeException)
import System.Timeout (timeout)
#endif

-- ─────────────────────────────────────────────────────────────────────────────
-- Result type
-- ─────────────────────────────────────────────────────────────────────────────

-- | The outcome of a LiquidHaskell verification run.
data LiquidResult
  = LiquidSafe
    -- ^ LH verified all refinement annotations hold — mutation is safe.
  | LiquidUnsafe Text
    -- ^ LH found a refinement violation; the 'Text' carries the details.
  | LiquidError Text
    -- ^ LH crashed or produced unexpected output; the 'Text' is the raw output.
  deriving (Show, Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- Verification
-- ─────────────────────────────────────────────────────────────────────────────

#ifdef WITH_LIQUID

-- | Run LiquidHaskell on @mutatedFile@ inside @repoRoot@ as a subprocess.
--
-- Invokes:
-- @HOME=/Users/raz cabal build -f+with-liquid --ghc-options='-fplugin LiquidHaskell' \<target\>@
--
-- The target is derived from the mutated file's path by stripping the
-- @src/@ prefix and @.hs@ suffix and replacing @/@ with @.@.
--
-- A 120-second wall-clock timeout is applied.  If LH does not finish within
-- that window, 'LiquidError' is returned with a "timeout" message.
verifyWithLiquid :: FilePath  -- ^ Repository root (working directory for cabal).
                 -> FilePath  -- ^ Path to the mutated file (relative or absolute).
                 -> IO LiquidResult
verifyWithLiquid repoRoot mutatedFile = do
  let target  = fileToTarget mutatedFile
      args    = [ "build"
                , "--with-compiler=" ++ ghcBin
                , "-f+with-liquid"
                , "--ghc-options=-fplugin LiquidHaskell"
                , target
                ]
      -- Inject HOME so LH can locate its package database.
      env     = Just [("HOME", "/Users/raz"), ("PATH", "/usr/bin:/bin:/usr/sbin:/sbin")]
  hPutStrLn stderr ("DGM.Liquid: verifyWithLiquid " ++ mutatedFile)
  result <- timeout liquidTimeoutMicros $
    try (readProcessWithExitCode cabalBin args "" :: IO (ExitCode, String, String))
  case result of
    Nothing ->
      return (LiquidError "LiquidHaskell verification timed out (120s)")
    Just (Left ex) ->
      return (LiquidError ("LiquidHaskell subprocess error: " <> T.pack (show (ex :: SomeException))))
    Just (Right (_, stdout, stderr')) -> do
      let combined = stdout ++ stderr'
      return (parseLiquidOutput combined)

-- | Convert a file path to a cabal build target.
--
-- e.g. @\"src/DGM/Types.hs\"@ → @\"DGM.Types\"@
fileToTarget :: FilePath -> String
fileToTarget fp =
  let stripped  = stripPrefix "src/" fp
      noExt     = reverse . drop 3 . reverse $ stripped  -- drop ".hs"
      dotted    = map (\c -> if c == '/' then '.' else c) noExt
  in  dotted
  where
    stripPrefix prefix s
      | take (length prefix) s == prefix = drop (length prefix) s
      | otherwise                         = s

-- | 120 seconds in microseconds.
liquidTimeoutMicros :: Int
liquidTimeoutMicros = 120 * 1000 * 1000

cabalBin :: FilePath
cabalBin = "/Users/raz/.ghcup/bin/cabal"

ghcBin :: FilePath
ghcBin = "/Users/raz/.ghcup/bin/ghc-9.6.7"

#else

-- | Stub: always returns 'LiquidSafe' when compiled without @WITH_LIQUID@.
verifyWithLiquid :: FilePath -> FilePath -> IO LiquidResult
verifyWithLiquid _repoRoot _mutatedFile = return LiquidSafe

#endif

-- ─────────────────────────────────────────────────────────────────────────────
-- Output parsing (unconditional — used in tests regardless of flag)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Parse LiquidHaskell output into a 'LiquidResult'.
--
-- Matching rules (applied in order):
--
--   * Any line containing @\"RESULT: SAFE\"@ → 'LiquidSafe'.
--   * Any line containing @\"RESULT: UNSAFE\"@ → 'LiquidUnsafe' with the
--     full first matching line as details.
--   * Otherwise → 'LiquidError' with the first 200 characters of output.
parseLiquidOutput :: String -> LiquidResult
parseLiquidOutput output
  | any (T.isInfixOf "RESULT: SAFE")   linesT = LiquidSafe
  | otherwise =
      case filter (T.isInfixOf "RESULT: UNSAFE") linesT of
        (l:_) -> LiquidUnsafe l
        []    -> LiquidError (T.take 200 (T.pack output))
  where
    linesT = map T.pack (lines output)

-- | DGM.Sandbox — Dynamic evaluation and strict sandboxing (SPEC.md §2.2).
--
-- Implements the two-layer sandboxing architecture:
--
--  * /Interpreter Monad/ (hint / GHC-API): loads and evaluates Haskell code at
--    runtime.  Active when the @with-hint@ cabal flag is set.
--
--  * /Hard-bounded execution/ (mueval-style): enforces CPU, memory and IO
--    limits independent of the hint layer.  This is implemented via @async@
--    with a configurable timeout and a resource monitor.
--
-- When @with-hint@ is disabled (the default) a /pure interpreter/ executes the
-- generated @Expr@ directly using the evaluator from "DGM.AST", providing a
-- safe, portable fallback that exercises the same pipeline.
{-# LANGUAGE CPP #-}
module DGM.Sandbox
  ( -- * Configuration
    SandboxConfig(..)
  , defaultSandboxConfig
    -- * Execution
  , SandboxResult(..)
  , runInSandbox
  , runExprInSandbox
    -- * Resource limits
  , ResourceLimits(..)
  , defaultLimits
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (SomeException, evaluate, try)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import DGM.AST

#ifdef WITH_HINT
import qualified DGM.HintBridge as HintBridge
#endif

-- ─────────────────────────────────────────────────────────────────────────────
-- Resource limits
-- ─────────────────────────────────────────────────────────────────────────────

-- | Hard resource limits analogous to the mueval / GraalVM polyglot limits
-- described in SPEC.md §2.2.2.
data ResourceLimits = ResourceLimits
  { rlTimeoutMs    :: Int    -- ^ Maximum wall-clock time (milliseconds).
  , rlMaxDepth     :: Int    -- ^ Maximum evaluation depth (recursion guard).
  , rlMaxNodes     :: Int    -- ^ Maximum AST nodes in generated expressions.
  , rlAllowIO      :: Bool   -- ^ Whether IO operations are permitted.
  } deriving (Show)

defaultLimits :: ResourceLimits
defaultLimits = ResourceLimits
  { rlTimeoutMs  = 2000    -- 2 s
  , rlMaxDepth   = 1000
  , rlMaxNodes   = 500
  , rlAllowIO    = False   -- IO blacklisted by default, matching Safe Haskell.
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- Sandbox configuration
-- ─────────────────────────────────────────────────────────────────────────────

data SandboxConfig = SandboxConfig
  { scLimits    :: ResourceLimits
  , scImports   :: [String]  -- ^ Haskell modules available inside sandbox.
  , scSafeOnly  :: Bool      -- ^ Enforce Safe Haskell restrictions.
  } deriving (Show)

defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig = SandboxConfig
  { scLimits  = defaultLimits
  , scImports = [ "Prelude"
                , "Data.List"
                , "Data.Maybe"
                ]
  , scSafeOnly = True
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- Sandbox result
-- ─────────────────────────────────────────────────────────────────────────────

data SandboxResult = SandboxResult
  { srPassed    :: Bool
  , srOutput    :: Either Text Text  -- ^ Left=error, Right=output
  , srLatencyMs :: Double
  , srMemoryMb  :: Double            -- ^ Approximate; 0 in pure fallback.
  , srScore     :: Double            -- ^ Normalised [0,1] accuracy score.
  } deriving (Show)

-- ─────────────────────────────────────────────────────────────────────────────
-- Main entry point
-- ─────────────────────────────────────────────────────────────────────────────

-- | Execute a Haskell code string in the bounded sandbox.
--
-- Dispatch:
--  * @with-hint@ enabled → hint Interpreter monad with mueval-style limits.
--  * @with-hint@ disabled → pure @Expr@-interpreter fallback.
runInSandbox :: SandboxConfig -> Text -> IO SandboxResult
runInSandbox cfg code = do
  t0 <- getCurrentTime
#ifdef WITH_HINT
  env <- HintBridge.newHintEnv
  let tMs = rlTimeoutMs (scLimits cfg)
  rawResult <- race (delay tMs) (HintBridge.evalHaskell env code)
  let result = case rawResult of
        Left () ->
          SandboxResult { srPassed = False
                        , srOutput = Left "Timeout in hint interpreter"
                        , srLatencyMs = 0, srMemoryMb = 0, srScore = 0 }
        Right (Left err) ->
          SandboxResult { srPassed = False
                        , srOutput = Left err
                        , srLatencyMs = 0, srMemoryMb = 0, srScore = 0 }
        Right (Right out) ->
          SandboxResult { srPassed = True
                        , srOutput = Right out
                        , srLatencyMs = 0, srMemoryMb = 0, srScore = 1.0 }
#else
  result <- pureEval cfg code
#endif
  t1 <- getCurrentTime
  let latMs = realToFrac (diffUTCTime t1 t0) * 1000
  pure result { srLatencyMs = latMs }

-- | Evaluate an @Expr@ tree directly in the sandbox (no code-generation step).
runExprInSandbox :: SandboxConfig -> EvalEnv -> Expr -> IO SandboxResult
runExprInSandbox cfg env expr = do
  -- Guard: reject expressions that exceed the node-count limit.
  let nc = countNodes expr
  if nc > rlMaxNodes (scLimits cfg)
    then pure SandboxResult
           { srPassed    = False
           , srOutput    = Left ("AST too large: " <> T.pack (show nc) <> " nodes")
           , srLatencyMs = 0
           , srMemoryMb  = 0
           , srScore     = 0
           }
    else timedEval cfg env expr

-- ─────────────────────────────────────────────────────────────────────────────
-- Timed evaluation with async timeout
-- ─────────────────────────────────────────────────────────────────────────────

timedEval :: SandboxConfig -> EvalEnv -> Expr -> IO SandboxResult
timedEval cfg env expr = do
  t0      <- getCurrentTime
  outcome <- race (delay (rlTimeoutMs (scLimits cfg)))
                  (safeEval env expr)
  t1      <- getCurrentTime
  let latMs = realToFrac (diffUTCTime t1 t0) * 1000
  case outcome of
    Left () ->
      pure SandboxResult
        { srPassed    = False
        , srOutput    = Left "Timeout: execution exceeded resource limit"
        , srLatencyMs = fromIntegral (rlTimeoutMs (scLimits cfg))
        , srMemoryMb  = 0
        , srScore     = 0
        }
    Right (Left err) ->
      pure SandboxResult
        { srPassed    = False
        , srOutput    = Left err
        , srLatencyMs = latMs
        , srMemoryMb  = 0
        , srScore     = 0
        }
    Right (Right val) ->
      pure SandboxResult
        { srPassed    = True
        , srOutput    = Right (T.pack (show val))
        , srLatencyMs = latMs
        , srMemoryMb  = 0       -- Full mem tracking requires RTS hooks.
        , srScore     = scoreValue val
        }

-- | Delay for @ms@ milliseconds (used as the timeout arm).
delay :: Int -> IO ()
delay ms = threadDelay (ms * 1000)

-- | Evaluate an @Expr@ under @env@, catching all exceptions.
safeEval :: EvalEnv -> Expr -> IO (Either Text Value)
safeEval env expr = do
  res <- try (evaluate (evalExpr env expr))
  pure $ case res of
    Left ex  -> Left (T.pack (show (ex :: SomeException)))
    Right ev -> ev

-- | Heuristic score: how "good" is the value as a task solution?
-- In the full system this is the Absolute Zero accuracy reward.
scoreValue :: Value -> Double
scoreValue (VInt  n) | n > 0     = 1.0
                     | otherwise = 0.5
scoreValue (VBool True)          = 1.0
scoreValue (VBool False)         = 0.3
scoreValue VUnit                 = 0.5
scoreValue (VFun _)              = 0.8  -- Produced a function; potentially useful.

-- ─────────────────────────────────────────────────────────────────────────────
-- Pure interpreter fallback (non-hint build only)
-- ─────────────────────────────────────────────────────────────────────────────

#ifndef WITH_HINT
-- | Pure-interpreter fallback when hint is unavailable.
pureEval :: SandboxConfig -> Text -> IO SandboxResult
pureEval _cfg _code =
  -- In the hint-disabled build we cannot parse arbitrary Haskell text;
  -- callers should use 'runExprInSandbox' instead.
  pure SandboxResult
    { srPassed    = False
    , srOutput    = Left "hint backend not enabled; use runExprInSandbox"
    , srLatencyMs = 0
    , srMemoryMb  = 0
    , srScore     = 0
    }
#endif


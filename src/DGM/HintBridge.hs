{-# LANGUAGE CPP #-}
-- | DGM.HintBridge — Persistent GHC interpreter wrapper (SPEC.md §2.2.1).
--
-- Provides a stable, reusable interface to the hint\/GHC-API backend.
-- The 'HintEnv' holds a long-lived interpreter session so callers avoid
-- per-call GHC initialisation overhead.
--
-- Compiled unconditionally: when the @with-hint@ cabal flag is absent,
-- every function returns a @'Left'@ explaining the backend is disabled.
module DGM.HintBridge
  ( HintEnv
  , newHintEnv
  , evalHaskell
  , typeCheckHaskell
  , evalRuleCandidate
  ) where

import Data.Text (Text)
import DGM.AST (ExprF(..))

#ifdef WITH_HINT
import qualified Data.Text as T
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Monad (forever)
import Language.Haskell.Interpreter
  ( runInterpreter, setImports, interpret, typeOf, as
  , reset, set, OptionVal((:=)), languageExtensions
  , Extension(Safe) )

-- ─────────────────────────────────────────────────────────────────────────────
-- Internal request type
-- ─────────────────────────────────────────────────────────────────────────────

data HintMode = EvalMode | TypeMode

data HintReq = HintReq
  { hrCode  :: String
  , hrMode  :: HintMode
  , hrReply :: MVar (Either String String)
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- Public API
-- ─────────────────────────────────────────────────────────────────────────────

-- | Handle to a persistent hint worker.
--
-- Backed by a long-lived background thread that serialises evaluation requests
-- via an internal 'Chan'.  Each request is evaluated in an isolated
-- 'runInterpreter' session so errors cannot contaminate subsequent calls.
-- Use 'bracket' (or similar) around 'newHintEnv' to kill the thread when the
-- env is no longer needed.
data HintEnv = HintEnv
  { heChan    :: Chan HintReq
  , heThread  :: ThreadId
  , heTimeout :: Int          -- ^ Hard wall-clock timeout (milliseconds).
  }

-- | Default allowed imports for sandboxed expressions.
-- Restricted to modules that are reliably available and safe-importable
-- across GHC versions.  Callers needing additional modules should construct
-- a configurable 'HintEnv' in future phases.
defaultImports :: [String]
defaultImports = ["Prelude", "Data.List", "Data.Maybe"]

-- | Imports allowed for rule-candidate evaluation.
--
-- Extends 'defaultImports' with @DGM.AST@ so that snippets can reference
-- @ExprF@ constructors.  The @Safe@ extension is intentionally omitted here;
-- instead, we enforce safety by restricting the available import list.
-- @System.IO.Unsafe@ and similar modules are not in scope, so unsafe IO
-- attempts will fail to compile.
ruleImports :: [String]
ruleImports = ["Prelude", "Data.List", "Data.Maybe", "DGM.AST"]

-- | Allocate a new persistent GHC interpreter session.
--
-- A background thread is forked; it owns the GHC interpreter and processes
-- requests serialised via the returned 'HintEnv'.  Safe Haskell is enforced
-- by default; only 'defaultImports' are visible inside the sandbox.
newHintEnv :: IO HintEnv
newHintEnv = do
  chan <- newChan
  tid  <- forkIO (runSession chan)
  pure HintEnv { heChan = chan, heThread = tid, heTimeout = 5000 }

-- | Evaluate a Haskell expression and return its @show@ed result as 'Text'.
--
-- The expression must have type @String@ (wrap with @show@ as needed).
-- Returns @'Left' err@ on compile error, runtime error, or timeout.
evalHaskell :: HintEnv -> Text -> IO (Either Text Text)
evalHaskell env code = dispatch env (T.unpack code) EvalMode

-- | Return the inferred GHC type of a Haskell expression as 'Text'.
--
-- Returns @'Left' err@ on error or timeout.
typeCheckHaskell :: HintEnv -> Text -> IO (Either Text Text)
typeCheckHaskell env code = dispatch env (T.unpack code) TypeMode

-- | Evaluate a Haskell snippet as an @ExprF Int -> ExprF Int@ rewrite rule.
--
-- The snippet is interpreted in a fresh GHC session with 'ruleImports'
-- available.  The @Safe@ extension is NOT applied here; safety is enforced by
-- restricting the import set so unsafe modules are simply not in scope.
--
-- A 5-second wall-clock timeout is applied.  Returns:
--
-- * @'Right' f@ — the snippet compiled and has the required type.
-- * @'Left' err@ — compile error, type mismatch, or timeout.
--
-- Example valid snippet: @\"\\\\x -> fmap (+1) x\"@
-- Example invalid snippet: @\"\\\\x -> True\"@  (wrong type)
evalRuleCandidate :: HintEnv -> Text -> IO (Either Text (ExprF Int -> ExprF Int))
evalRuleCandidate HintEnv{..} code = do
  outcome <- race
    (threadDelay (heTimeout * 1000))
    (runInterpreter $ do
       reset
       setImports ruleImports
       -- No Safe extension: import-list restriction is our safety mechanism.
       interpret (T.unpack code) (as :: ExprF Int -> ExprF Int))
  pure $ case outcome of
    Left  ()          -> Left "Timeout evaluating rule candidate"
    Right (Left  err) -> Left (T.pack (show err))
    Right (Right f)   -> Right f

-- ─────────────────────────────────────────────────────────────────────────────
-- Internal: send a single request to the background session
-- ─────────────────────────────────────────────────────────────────────────────

dispatch :: HintEnv -> String -> HintMode -> IO (Either Text Text)
dispatch HintEnv{..} code mode = do
  replyVar <- newEmptyMVar
  outcome  <- race
    (threadDelay (heTimeout * 1000))
    (writeChan heChan (HintReq code mode replyVar) >> takeMVar replyVar)
  pure $ case outcome of
    Left  ()          -> Left "Timeout in hint interpreter"
    Right (Left  err) -> Left  (T.pack err)
    Right (Right val) -> Right (T.pack val)

-- ─────────────────────────────────────────────────────────────────────────────
-- Internal: background interpreter session
-- ─────────────────────────────────────────────────────────────────────────────

-- | Long-lived worker loop.
--
-- The background thread persists for the lifetime of the 'HintEnv', processing
-- requests one at a time.  Each request runs its own 'runInterpreter' session
-- so that errors are fully isolated: a failed compile does not contaminate the
-- next call.  'bracket'-style cleanup is automatic via hint's own finaliser.
runSession :: Chan HintReq -> IO ()
runSession chan = forever $ do
  req    <- readChan chan
  result <- runInterpreter $ do
    reset
    setImports defaultImports
    set [languageExtensions := [Safe]]
    case hrMode req of
      EvalMode -> Right <$> interpret (hrCode req) (as :: String)
      TypeMode -> Right <$> typeOf   (hrCode req)
  putMVar (hrReply req) $ case result of
    Left  err -> Left  (show err)
    Right val -> val

#else
-- ─────────────────────────────────────────────────────────────────────────────
-- Stub (with-hint flag absent)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Opaque stub — hint backend disabled at build time.
data HintEnv = HintEnv

-- | No-op: returns a stub env immediately.
newHintEnv :: IO HintEnv
newHintEnv = pure HintEnv

-- | Always returns 'Left' when hint is not compiled in.
evalHaskell :: HintEnv -> Text -> IO (Either Text Text)
evalHaskell _ _ = pure (Left "hint backend not enabled; build with -f+with-hint")

-- | Always returns 'Left' when hint is not compiled in.
typeCheckHaskell :: HintEnv -> Text -> IO (Either Text Text)
typeCheckHaskell _ _ = pure (Left "hint backend not enabled; build with -f+with-hint")

-- | Always returns 'Left' when hint is not compiled in.
evalRuleCandidate :: HintEnv -> Text -> IO (Either Text (ExprF Int -> ExprF Int))
evalRuleCandidate _ _ = pure (Left "hint backend not enabled; build with -f+with-hint")
#endif

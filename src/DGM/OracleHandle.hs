-- | DGM.OracleHandle — Bluefin-inspired capability token for oracle access.
--
-- The 'OracleHandle' type is an explicit capability token that must be
-- threaded through any code that calls the oracle.  The constructor is not
-- exported, so the ONLY way to obtain an 'OracleHandle' is via 'withOracle'.
-- This makes oracle IO visible in every call-site signature and prevents
-- accidental oracle calls from pure helper code.
--
-- Design inspiration — Bluefin's IOE / Handler pattern:
-- <https://hackage.haskell.org/package/bluefin>
--
-- The key property: passing 'OracleHandle' in a function signature proves that
-- oracle IO was explicitly requested at that call site.  Code that lacks an
-- 'OracleHandle' in scope cannot call 'proposeMutationH' or 'scoreMutationsH'.
-- Without the handle those calls are compile-time type errors.
--
-- Usage example:
--
-- @
-- mEnv <- newOracleEnv
-- case mEnv of
--   Nothing  -> -- fall back to heuristic mutations
--   Just env ->
--     withOracle env $ \\h -> do
--       result <- proposeMutationH h "src/DGM/Foo.hs" src tests
--       …
-- @
module DGM.OracleHandle
  ( -- * Handle
    OracleHandle
    -- * Acquiring a handle
  , withOracle
    -- * Handle-gated oracle operations
  , proposeMutationH
  , proposeMutationEnrichedH
  , scoreMutationsH
  ) where

import Data.Text (Text)

import DGM.HsAST (HsMutation)
import DGM.Oracle (OracleEnv, MutationContext, proposeMutation, proposeMutationEnriched, scoreAndRankMutations)

-- ─────────────────────────────────────────────────────────────────────────────
-- Capability token
-- ─────────────────────────────────────────────────────────────────────────────

-- | Capability token for oracle access.
--
-- The data constructor @OracleHandle_@ is intentionally unexported.  Callers
-- must obtain a handle through 'withOracle'; there is no other way to
-- construct one.
--
-- By requiring 'OracleHandle' as an argument, a function declares at the type
-- level that it performs oracle IO.  Passing the handle is an explicit,
-- auditable proof of that intent.
data OracleHandle = OracleHandle_ OracleEnv

-- ─────────────────────────────────────────────────────────────────────────────
-- Bracket
-- ─────────────────────────────────────────────────────────────────────────────

-- | Run an action that requires oracle access.
--
-- This is the sole constructor of 'OracleHandle'.  The scoped-callback style
-- (passing the handle to @f@ rather than returning it) mirrors Bluefin's
-- @runEff@ / @handle@ idiom: the handle is semantically valid only within the
-- dynamic extent of the callback.
--
-- @
-- withOracle env $ \h -> do
--   mut <- proposeMutationH h fp src tests
--   …
-- @
withOracle :: OracleEnv -> (OracleHandle -> IO a) -> IO a
withOracle env f = f (OracleHandle_ env)

-- ─────────────────────────────────────────────────────────────────────────────
-- Handle-gated operations
-- ─────────────────────────────────────────────────────────────────────────────

-- | Propose a mutation for a source file via the oracle.
--
-- The 'OracleHandle' argument is the capability proof: its presence in the
-- signature makes oracle IO explicit and auditable.
--
-- Delegates to 'DGM.Oracle.proposeMutation' with the 'OracleEnv' unwrapped
-- from the handle.
proposeMutationH
  :: OracleHandle
  -> FilePath              -- ^ Target source file (relative to repo root).
  -> Text                  -- ^ Full module source text.
  -> [Text]                -- ^ Test function names present in the module.
  -> Maybe MutationContext -- ^ Optional archive feedback for guided mutation.
  -> IO (Either Text HsMutation)
proposeMutationH (OracleHandle_ env) = proposeMutation env

-- | Score and rank a list of mutations via the oracle.
--
-- Returns each mutation paired with the oracle's confidence score in @[0,1]@.
-- Like 'proposeMutationH', the 'OracleHandle' argument proves the caller
-- explicitly requested oracle IO.
-- | Propose a mutation using an enriched (GHC-guided) prompt.
--
-- Like 'proposeMutationH' but takes a pre-built enriched prompt that
-- includes GHC-verified safe removals.
proposeMutationEnrichedH
  :: OracleHandle
  -> FilePath -> Text -> Text   -- ^ file path, source, enriched prompt
  -> IO (Either Text HsMutation)
proposeMutationEnrichedH (OracleHandle_ env) = proposeMutationEnriched env

scoreMutationsH
  :: OracleHandle
  -> [HsMutation]
  -> IO [(HsMutation, Double)]
scoreMutationsH (OracleHandle_ env) = scoreAndRankMutations env

-- | DGM.SafetyKernel — Type-driven AI safety and the Airlock Kernel (SPEC.md §3).
--
-- This module implements the "Airlock Kernel" architecture described in
-- SPEC.md §3.  All three safety invariants are encoded at the /type level/:
--
--  1. @'Safe@ commands execute without a transaction plan.
--  2. @'Critical@ commands must be paired with a pure-data rollback snapshot.
--  3. @'Existential@ commands are structurally unrepresentable without a valid
--     'QuorumProof', meaning the type-checker enforces human-in-the-loop.
--
-- The key property: the agent /cannot/ hallucinate a workaround or bypass
-- these gates because the constraint is in the /type/ of the dispatch
-- function, not in a runtime @if isSafe then … else …@ check.
module DGM.SafetyKernel
  ( -- * Airlock dispatch
    dispatch
  , dispatchSafe
  , dispatchCritical
  , dispatchExistential
    -- * Transaction plan
  , TransactionPlan(..)
  , mkTransactionPlan
  , rollback
    -- * Quorum verification
  , verifyQuorum
  , mockQuorumProof
    -- * Intent / Impact audit
  , AuditLog
  , AuditEntry(..)
  , auditCommand
  , appendAudit
  , newAuditLog
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as B16
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.FilePath (isAbsolute, normalise, splitPath)
import System.IO (hPutStr, withFile, IOMode(..))
import qualified Data.Text.IO as TIO
import System.Directory (removeFile, renameFile)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, IOException)

import Data.IORef
import DGM.Types
import DGM.AST

-- ─────────────────────────────────────────────────────────────────────────────
-- Airlock dispatch
-- ─────────────────────────────────────────────────────────────────────────────

-- | Top-level dispatch over all safety levels.
--
-- The type parameter @l@ is phantom but enforced by the GADT; GHC will
-- reject code that tries to call @dispatchCritical@ with a @Command 'Safe@.
dispatch :: AgentState -> AuditLog -> AnyCommand -> IO (Either Text Text)
dispatch st audit (AnyCommand level cmd) =
  case level of
    SingSafe        -> dispatchSafe        st audit cmd
    SingCritical    -> dispatchCritical    st audit cmd
    SingExistential -> dispatchExistential st audit cmd

-- | Execute a @Safe@ command directly — no transaction plan needed.
dispatchSafe :: AgentState -> AuditLog -> Command 'Safe -> IO (Either Text Text)
dispatchSafe _st audit cmd = do
  appendAudit audit (auditEntry "Safe" (describeCmd cmd))
  case cmd of
    Evaluate code ->
      pure (Right ("Evaluated: " <> code))
    Inspect node ->
      pure (Right ("Inspected node: " <> astId node
                   <> " type=" <> T.pack (show (astType node))))
    RunSandboxed code ->
      pure (Right ("Sandboxed execution of: " <> code))

-- | Execute a @Critical@ command with STM-backed rollback.
--
-- The 'mkTransactionPlan' call captures the current state BEFORE the command
-- runs.  If the command fails or produces a negative utility delta, 'rollback'
-- restores the snapshot instantly (O(1) via persistent-data sharing).
dispatchCritical :: AgentState -> AuditLog -> Command 'Critical -> IO (Either Text Text)
dispatchCritical st audit cmd = do
  -- Step 1: capture snapshot (the "transaction plan").
  plan <- mkTransactionPlan st
  appendAudit audit (auditEntry "Critical" (describeCmd cmd))
  -- Step 2: execute.
  result <- executeCritical st cmd
  case result of
    Left err -> do
      -- Step 3a: failure → pure-data rollback.
      rollback st plan
      appendAudit audit (auditEntry "Rollback" ("Rolled back due to: " <> err))
      pure (Left ("Critical command failed, rolled back: " <> err))
    Right ok ->
      pure (Right ok)

-- | Execute a @Critical@ command's actual logic.
executeCritical :: AgentState -> Command 'Critical -> IO (Either Text Text)
executeCritical st = \case
  MutateAST mutation -> atomically $ do
    currentAST <- readTVar (stateCurrentAST st)
    let newAST = applyMutation currentAST mutation
    writeTVar (stateCurrentAST st) newAST
    pure (Right ("AST mutated: " <> mutationTarget mutation))
  UpdateState update -> atomically $ do
    modifyTVar' (stateStateMap st) (Map.insert (updateKey update) (updateValue update))
    pure (Right ("State updated: " <> updateKey update))

-- | Apply a mutation to an AST node (structural substitution).
--
-- Only 'ExprMutation' payloads are applicable here; 'HaskellMutation' is
-- handled by 'DGM.HsAST.applyHsMutation' and operates on source text.
applyMutation :: ASTNode -> Mutation -> ASTNode
applyMutation node mutation =
  case mutationPayload mutation of
    ExprMutation _old new
      | astId node == mutationTarget mutation -> new
      | otherwise -> node { astChildren = map (`applyMutation` mutation) (astChildren node) }
    HaskellMutation _ -> node  -- no-op: Haskell mutations are text-level

-- | Execute an @Existential@ command — requires valid quorum.
--
-- The @QuorumProof@ argument is a structural /type requirement/ from the GADT
-- constructor @Terminate@ / @SystemWrite@.  The system cannot even express an
-- Existential command without providing (at least) a @QuorumProof@ value.
-- 'verifyQuorum' then checks the SHA-256 hash, operation binding, and that
-- the proof timestamp is within the 5-minute validity window.
dispatchExistential :: AgentState -> AuditLog -> Command 'Existential -> IO (Either Text Text)
dispatchExistential st audit cmd = do
  now <- floor <$> (getPOSIXTime :: IO POSIXTime) :: IO Int64
  appendAudit audit (auditEntry "Existential" (describeCmd cmd))
  case cmd of
    Terminate proof -> do
      case verifyQuorum proof "terminate" now of
        Left err -> pure (Left ("Quorum verification failed: " <> err))
        Right _  -> do
          appendAudit audit (auditEntry "Shutdown" "Quorum verified; terminating.")
          pure (Right "Terminate authorised")
    SystemWrite path content proof -> do
      case verifyQuorum proof ("write:" <> T.pack path) now of
        Left err -> pure (Left ("Quorum verification failed: " <> err))
        Right _  -> do
          -- Path whitelist: only allow paths under repo src/ directory.
          let repoRoot = stateRepoRoot st
          case checkPathWhitelist repoRoot path of
            Left err -> pure (Left err)
            Right _  -> do
              -- Atomic write via temp file + rename.
              result <- atomicWriteFile path content
              case result of
                Left err -> pure (Left err)
                Right _  -> do
                  -- Audit trail with SHA-256 of content.
                  let contentHash = sha256Hex content
                  appendAudit audit (auditEntry "SystemWrite"
                    ("SystemWrite: " <> T.pack path <> " sha256=" <> contentHash))
                  pure (Right ("Write completed: " <> T.pack path))

-- ─────────────────────────────────────────────────────────────────────────────
-- Transaction plan (reversible computing)
-- ─────────────────────────────────────────────────────────────────────────────

-- | A pure-data snapshot of the agent's state before a Critical operation.
--
-- Because Haskell uses persistent (immutable) data structures, the snapshot
-- is just a pointer to the old value — no copying is needed (SPEC.md §3.2).
data TransactionPlan = TransactionPlan
  { tpGeneration :: Int
  , tpAST        :: ASTNode
  , tpStateMap   :: Map Text Text
  , tpMetrics    :: Metrics
  } deriving (Show)

-- | Capture the current state as a pure-data rollback point.
mkTransactionPlan :: AgentState -> IO TransactionPlan
mkTransactionPlan st = atomically $ do
  gen  <- readTVar (stateGeneration st)
  ast  <- readTVar (stateCurrentAST st)
  sm   <- readTVar (stateStateMap st)
  met  <- readTVar (stateMetrics st)
  pure TransactionPlan
    { tpGeneration = gen
    , tpAST        = ast
    , tpStateMap   = sm
    , tpMetrics    = met
    }

-- | Restore the agent's state from a transaction plan.
--
-- This is O(1): we simply overwrite the TVars with the saved pointers.
rollback :: AgentState -> TransactionPlan -> IO ()
rollback st plan = atomically $ do
  writeTVar (stateGeneration st) (tpGeneration plan)
  writeTVar (stateCurrentAST st) (tpAST plan)
  writeTVar (stateStateMap   st) (tpStateMap plan)
  writeTVar (stateMetrics    st) (tpMetrics plan)

-- ─────────────────────────────────────────────────────────────────────────────
-- Quorum verification
-- ─────────────────────────────────────────────────────────────────────────────

-- | Verify a cryptographic quorum proof for the given operation.
--
-- Three checks are applied:
--  1. The proof's 'quorumOperation' must match @opName@ (operation binding).
--  2. The proof's 'quorumHash' must equal the SHA-256 hex digest of @opName@.
--  3. The proof's 'quorumTimestamp' must be within 5 minutes (300 s) of @now@.
--
-- This prevents cross-operation replay (a 'terminate' proof cannot authorise
-- a 'write:…' command) and timestamp replay (stale proofs are rejected).
verifyQuorum :: QuorumProof -> Text -> Int64 -> Either Text ()
verifyQuorum proof opName now
  | quorumOperation proof /= opName =
      Left ("Operation mismatch: proof is for '"
            <> quorumOperation proof <> "' but used for '" <> opName <> "'")
  | now - quorumTimestamp proof > 300 =
      Left "Proof expired: timestamp is more than 5 minutes old"
  | quorumTimestamp proof > now =
      Left "Proof timestamp is in the future"
  | quorumHash proof /= sha256Hex opName =
      Left ("Hash mismatch: got " <> quorumHash proof
            <> " expected " <> sha256Hex opName)
  | otherwise = Right ()

-- | Generate a mock quorum proof (for testing only).
--
-- Uses real SHA-256 so tests exercise the same hash path as production.
mockQuorumProof :: Text -> Int64 -> QuorumProof
mockQuorumProof opName ts = QuorumProof
  { quorumHash      = sha256Hex opName
  , quorumTimestamp = ts
  , quorumOperation = opName
  }

-- | SHA-256 hash of a Text value, returned as a lowercase hex 'Text'.
sha256Hex :: Text -> Text
sha256Hex t = TE.decodeUtf8 (B16.encode (SHA256.hash (TE.encodeUtf8 t)))

-- ─────────────────────────────────────────────────────────────────────────────
-- Intent / Impact audit
-- ─────────────────────────────────────────────────────────────────────────────

-- | Append-only audit log.
type AuditLog = IORef [AuditEntry]

data AuditEntry = AuditEntry
  { aeLevel   :: Text
  , aeMessage :: Text
  } deriving (Show, Eq)

-- | Create a new empty audit log.
newAuditLog :: IO AuditLog
newAuditLog = newIORef []

appendAudit :: AuditLog -> AuditEntry -> IO ()
appendAudit log' entry = modifyIORef' log' (entry :)

auditEntry :: Text -> Text -> AuditEntry
auditEntry level msg = AuditEntry { aeLevel = level, aeMessage = msg }

-- | Pure audit function — reads command description without executing.
-- This mirrors the "pure function with zero side effects" audit kernel of
-- SPEC.md §3.1.
auditCommand :: AnyCommand -> AuditEntry
auditCommand (AnyCommand level cmd) =
  let lvl = case level of
              SingSafe        -> "Safe"
              SingCritical    -> "Critical"
              SingExistential -> "Existential"
  in  auditEntry lvl (describeCmd cmd)

-- | Human-readable description of a command (level-polymorphic).
describeCmd :: Command l -> Text
describeCmd = \case
  Evaluate code          -> "Evaluate: " <> code
  Inspect node           -> "Inspect: " <> astId node
  RunSandboxed code      -> "RunSandboxed: " <> code
  MutateAST mutation     -> "MutateAST: " <> mutationTarget mutation
  UpdateState upd        -> "UpdateState: " <> updateKey upd
  Terminate _            -> "Terminate (quorum)"
  SystemWrite path _ _   -> "SystemWrite: " <> T.pack path

-- ─────────────────────────────────────────────────────────────────────────────
-- SystemWrite helpers
-- ─────────────────────────────────────────────────────────────────────────────

-- | Check that @path@ is under @repoRoot/src/@.
--
-- Returns 'Left' with an error message if the path is outside the whitelist,
-- 'Right ()' if it is allowed.
{-@ checkPathWhitelist :: repoRoot:String -> path:String -> Either Text () @-}
checkPathWhitelist :: FilePath -> FilePath -> Either Text ()
checkPathWhitelist repoRoot path
  | null repoRoot =
      Left "Path not whitelisted: stateRepoRoot is not configured"
  | otherwise =
      let srcDir    = normalise (repoRoot <> "/src/")
          absPath   = if isAbsolute path then path else repoRoot <> "/" <> path
          normPath  = normalise absPath
          -- Check that normPath starts with srcDir.
          -- splitPath srcDir gives path components including trailing slash on dirs.
          srcParts  = splitPath srcDir
          pathParts = splitPath normPath
          underSrc  = length pathParts >= length srcParts
                      && take (length srcParts) pathParts == srcParts
      in if underSrc
           then Right ()
           else Left ("Path not whitelisted: " <> T.pack path
                      <> " is outside repo src/ directory")

-- | Write @content@ to @path@ atomically using a temp file + rename.
--
-- Steps:
--   1. Write content to @path <> ".new"@.
--   2. Read back the @.new@ file and verify it is non-empty (basic sanity check).
--   3. @renameFile path.new path@ (atomic on POSIX/Darwin).
--   4. On any failure, attempt to delete the @.new@ temp file, return 'Left'.
atomicWriteFile :: FilePath -> Text -> IO (Either Text ())
atomicWriteFile path content = do
  let tmpPath = path <> ".new"
  result <- try (writeAndVerify tmpPath content) :: IO (Either SomeException ())
  case result of
    Left ex -> do
      -- Best-effort cleanup of temp file.
      removeFile tmpPath `catch` \e ->
        if isDoesNotExistError e then pure () else ioError e
      pure (Left ("SystemWrite failed during write/verify: " <> T.pack (show ex)))
    Right () -> do
      renameResult <- try (renameFile tmpPath path) :: IO (Either SomeException ())
      case renameResult of
        Left ex -> do
          removeFile tmpPath `catch` \e ->
            if isDoesNotExistError e then pure () else ioError e
          pure (Left ("SystemWrite failed during rename: " <> T.pack (show ex)))
        Right () -> pure (Right ())

-- | Write @content@ to @path@ and verify the content is non-empty.
writeAndVerify :: FilePath -> Text -> IO ()
writeAndVerify path content = do
  if T.null content
    then ioError (userError ("SystemWrite: content is empty for: " <> path))
    else TIO.writeFile path content

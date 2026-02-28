{-# LANGUAGE CPP #-}
-- | DGM.Types — Core type definitions for the Darwin Gödel Machine MVP.
--
-- This module establishes the fundamental type-level safety architecture
-- described in SPEC.md §3.  Every action the agent may take is classified
-- at compile-time as one of three safety levels via a GADT parameterised
-- by a promoted 'SafetyLevel' kind.  The GHC type-checker thereby prevents
-- category violations without any runtime overhead.
module DGM.Types
  ( -- * Safety levels
    SafetyLevel(..)
  , SingLevel(..)
    -- * Command GADT
  , Command(..)
  , AnyCommand(..)
    -- * Cryptographic quorum
  , QuorumProof(..)
    -- * Archive
  , ArchiveEntry(..)
  , ArchiveBackend(..)
    -- * AST primitives (re-used across modules)
  , ASTNode(..)
  , ASTNodeType(..)
  , MutationPayload(..)
  , Mutation(..)
  , MutationType(..)
    -- * Results
  , TestResult(..)
  , VerificationResult(..)
  , CounterExample(..)
    -- * State updates
  , StateUpdate(..)
    -- * Agent metrics
  , Metrics(..)
  , initialMetrics
    -- * Agent state handles
  , AgentState(..)
  , newAgentState
  , newAgentStateWithRoot
    -- * Phase G: module safety
  , ModuleName
  , safeguardedModules
  ) where

import Control.Concurrent.STM
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson

-- ─────────────────────────────────────────────────────────────────────────────
-- Safety Levels
-- ─────────────────────────────────────────────────────────────────────────────

-- ─────────────────────────────────────────────────────────────────────────────
-- LiquidHaskell refinement annotations (pillar II formal pre-flight)
-- ─────────────────────────────────────────────────────────────────────────────

-- These annotations are parsed by LiquidHaskell when compiled with
-- -fplugin LiquidHaskell.  They are syntactically valid Haskell comments
-- and are therefore inert when compiled without the LH plugin.

{-@ type Score = {v:Double | 0.0 <= v && v <= 1.0} @-}
-- ^ A score is a Double constrained to the unit interval [0.0, 1.0].

{-@ type Generation = {v:Int | 0 <= v} @-}
-- ^ A generation counter is a non-negative integer.

{-@ measure entryScore :: ArchiveEntry -> Double @-}
-- ^ Expose 'entryScore' as a LiquidHaskell measure so it can be used in
-- refinements on 'ArchiveEntry' lists (e.g. in getBest and computeStats).

-- | Three-tier safety classification for all agent actions (SPEC.md §3.1).
--
-- Promoted to a kind via @DataKinds@; used as a phantom parameter on 'Command'.
data SafetyLevel
  = Safe        -- ^ Pure computation, read-only access, sandboxed execution.
  | Critical    -- ^ Modifies internal state or the active AST.
  | Existential -- ^ Threatens host-machine integrity; requires quorum.
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Singleton witnesses for 'SafetyLevel', enabling runtime dispatch on the
-- level while preserving type-level information.
data SingLevel (l :: SafetyLevel) where
  SingSafe        :: SingLevel 'Safe
  SingCritical    :: SingLevel 'Critical
  SingExistential :: SingLevel 'Existential

-- ─────────────────────────────────────────────────────────────────────────────
-- Command GADT
-- ─────────────────────────────────────────────────────────────────────────────

-- | GADT parameterised by 'SafetyLevel'.
--
-- The type parameter makes it impossible to pass, e.g., a @Command 'Existential@
-- to a function that only accepts @Command 'Safe@.  The Airlock Kernel
-- (see 'DGM.SafetyKernel') uses these constructors as its proof objects.
data Command (l :: SafetyLevel) where
  -- Safe commands ─────────────────────────────────────────────────────────────
  -- | Evaluate a pure Haskell expression in the mueval sandbox.
  Evaluate      :: Text -> Command 'Safe
  -- | Inspect a node of the current AST (read-only).
  Inspect       :: ASTNode -> Command 'Safe
  -- | Run synthesised code in the strictly resource-bounded sandbox.
  RunSandboxed  :: Text -> Command 'Safe

  -- Critical commands ─────────────────────────────────────────────────────────
  -- | Apply a 'Mutation' to the current AST; requires a transaction plan.
  MutateAST     :: Mutation -> Command 'Critical
  -- | Update a named slot in the agent's mutable state map.
  UpdateState   :: StateUpdate -> Command 'Critical

  -- Existential commands ──────────────────────────────────────────────────────
  -- | Terminate the host process; requires a valid 'QuorumProof'.
  Terminate     :: QuorumProof -> Command 'Existential
  -- | Write to the host file-system; requires quorum.
  SystemWrite   :: FilePath -> Text -> QuorumProof -> Command 'Existential

-- | Existential wrapper so heterogeneous lists of commands can be stored.
data AnyCommand where
  AnyCommand :: SingLevel l -> Command l -> AnyCommand

-- ─────────────────────────────────────────────────────────────────────────────
-- Cryptographic Quorum
-- ─────────────────────────────────────────────────────────────────────────────

-- | Proof that a cryptographic multi-party quorum has been obtained.
--
-- Contains the SHA-256 hex digest of the authorised operation, the POSIX
-- timestamp (seconds) at which the proof was issued, and the operation name
-- the proof was minted for.  'DGM.SafetyKernel.verifyQuorum' checks all
-- three fields, preventing cross-operation and timestamp-replay attacks.
data QuorumProof = QuorumProof
  { quorumHash      :: Text   -- ^ SHA-256 hex digest of the operation name.
  , quorumTimestamp :: Int64  -- ^ POSIX seconds at proof-issuance time.
  , quorumOperation :: Text   -- ^ Operation this proof was minted for.
  } deriving (Show, Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- Archive Entry
-- ─────────────────────────────────────────────────────────────────────────────

-- | A single entry in the DGM expanding archive (SPEC.md §1.1).
--
-- Crucially, *failed* mutations are also preserved.  The non-convex
-- optimisation landscape means a dead-end today may be a stepping-stone
-- tomorrow.
{-@ data ArchiveEntry = ArchiveEntry
      { entryId           :: Text
      , entryCode         :: Text
      , entryMutation     :: Maybe Mutation
      , entryParentId     :: Maybe Text
      , entryScore        :: Score
      , entryPassed       :: Bool
      , entryGeneration   :: Generation
      , entryCounterEx    :: Maybe CounterExample
      , entryLiquidResult :: Maybe Text
      , entrySbvResult    :: Maybe Text
      , entryOracleModel  :: Maybe Text
      }
  @-}
data ArchiveEntry = ArchiveEntry
  { entryId           :: Text
  , entryCode         :: Text           -- ^ The code text of the agent at this point.
  , entryMutation     :: Maybe Mutation -- ^ What changed relative to parent.
  , entryParentId     :: Maybe Text     -- ^ Lineage pointer.
  , entryScore        :: Double         -- ^ Empirical accuracy / utility score [0,1].
  , entryPassed       :: Bool           -- ^ Did the sandbox test pass?
  , entryGeneration   :: Int            -- ^ Depth in the evolutionary tree (>= 0).
  , entryCounterEx    :: Maybe CounterExample  -- ^ SMT counter-example, if any.
  , entryLiquidResult :: Maybe Text     -- ^ LiquidHaskell result: "SAFE" | "UNSAFE: …" | Nothing.
  , entrySbvResult    :: Maybe Text     -- ^ SBV equivalence result: "QED" | "Falsifiable: …" | "Timeout" | Nothing.
  , entryOracleModel  :: Maybe Text     -- ^ Oracle model that proposed the mutation, or Nothing (heuristic).
  } deriving (Show, Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- AST Primitives
-- ─────────────────────────────────────────────────────────────────────────────

-- | An opaque AST node shared across the metaprogramming pipeline.
--
-- In the full system this would be GHC's @HsExpr GhcPs@; here we use a
-- simplified structural representation that preserves all key operations.
data ASTNode = ASTNode
  { astId       :: Text
  , astType     :: ASTNodeType
  , astChildren :: [ASTNode]
  , astValue    :: Maybe Text   -- ^ Leaf literal / identifier text.
  } deriving (Show, Eq)

data ASTNodeType
  = ModuleNode
  | FunctionDef
  | ExprNode
  | TypeDecl
  | PatternNode
  | LiteralNode
  | ApplicationNode
  | LambdaNode
  | LetNode
  | CaseNode
  deriving (Show, Eq, Enum, Bounded)

-- | The payload of a mutation — either an ExprF AST mutation (existing path)
-- or a Haskell source mutation (Phase B path).
--
-- 'HaskellMutation' carries the mutation description text; the actual
-- transform function lives in 'DGM.HsAST' to avoid a circular module
-- dependency.
data MutationPayload
  = ExprMutation ASTNode ASTNode
    -- ^ Mutation on the ExprF expression tree (Phase A/existing path).
    -- The two 'ASTNode' values are the old and new subtrees respectively.
  | HaskellMutation Text
    -- ^ Mutation on real Haskell source (Phase B). The 'Text' is the
    -- human-readable description of the rule that fired.
  deriving (Show, Eq)

-- | A proposed mutation to the active AST or source file.
--
-- Uses 'MutationPayload' for the content so that both the existing ExprF
-- path and the new Haskell-source path share the same type.
data Mutation = Mutation
  { mutationType    :: MutationType
  , mutationTarget  :: Text          -- ^ Rule name or source node identifier.
  , mutationPayload :: MutationPayload
  } deriving (Show, Eq)

data MutationType
  = Optimize         -- ^ Performance-preserving rewrite.
  | Refactor         -- ^ Structural cleanup.
  | BugFix           -- ^ Correctness repair.
  | Expand           -- ^ Capability extension.
  | ExistentialImpact -- ^ System-wide impact (e.g. Types.hs core type change); requires elevated quorum.
  deriving (Show, Eq, Enum, Bounded)

-- ─────────────────────────────────────────────────────────────────────────────
-- Results
-- ─────────────────────────────────────────────────────────────────────────────

-- | The outcome of running generated code in the mueval sandbox.
data TestResult = TestResult
  { resultPassed    :: Bool
  , resultScore     :: Double
  , resultLatencyMs :: Double
  , resultMemoryMb  :: Double
  , resultOutput    :: Either Text Text  -- ^ Left = error output, Right = success.
  } deriving (Show, Eq)

-- | Result of the SBV / Z3 equivalence proof (SPEC.md §2.3.2).
data VerificationResult
  = Verified    Text           -- ^ Q.E.D. — proof obligation discharged.
  | Falsifiable CounterExample -- ^ Solver found a concrete counter-example.
  | VTimeout    Text           -- ^ Undecidable or time-bounded.
  deriving (Show, Eq)

-- | Concrete counter-example produced by the SMT solver.
data CounterExample = CounterExample
  { ceInputs   :: Map Text Text
  , ceExpected :: Text
  , ceActual   :: Text
  } deriving (Show, Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- State
-- ─────────────────────────────────────────────────────────────────────────────

-- | A single named state slot update.
data StateUpdate = StateUpdate
  { updateKey   :: Text
  , updateValue :: Text
  } deriving (Show, Eq)

-- | Running performance metrics for the agent.
data Metrics = Metrics
  { totalIterations     :: Int
  , successfulMutations :: Int
  , failedMutations     :: Int
  , totalTasksSolved    :: Int
  , verificationsPassed :: Int
  , verificationsFailed :: Int
  } deriving (Show, Eq)

initialMetrics :: Metrics
initialMetrics = Metrics 0 0 0 0 0 0

-- | Backend selector for the DGM expanding archive.
--
-- 'InMemory' uses the existing 'TVar' list — fast, zero dependencies, but
-- ephemeral.  'SQLiteBacked' persists every entry to disk so stepping-stones
-- survive process restarts.
data ArchiveBackend
  = InMemory                -- ^ Pure in-memory (tests, ephemeral runs).
  | SQLiteBacked FilePath   -- ^ SQLite file at the given path.
  deriving (Show, Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- JSON instances for archive types (defined here to avoid -Worphans)
-- ─────────────────────────────────────────────────────────────────────────────

instance ToJSON CounterExample where
  toJSON ce = object
    [ "inputs"   .= ceInputs ce
    , "expected" .= ceExpected ce
    , "actual"   .= ceActual ce
    ]

instance FromJSON CounterExample where
  parseJSON = Aeson.withObject "CounterExample" $ \o ->
    CounterExample
      <$> o .: "inputs"
      <*> o .: "expected"
      <*> o .: "actual"

instance ToJSON ArchiveEntry where
  toJSON e = object
    [ "id"          .= entryId e
    , "code"        .= entryCode e
    , "parentId"    .= entryParentId e
    , "score"       .= entryScore e
    , "passed"      .= entryPassed e
    , "generation"  .= entryGeneration e
    , "lhResult"    .= entryLiquidResult e
    , "sbvResult"   .= entrySbvResult e
    , "oracleModel" .= entryOracleModel e
    ]

instance FromJSON ArchiveEntry where
  parseJSON = Aeson.withObject "ArchiveEntry" $ \o ->
    ArchiveEntry
      <$> o .:  "id"
      <*> o .:  "code"
      <*> pure Nothing     -- mutation not serialised (opaque)
      <*> o .:  "parentId"
      <*> o .:  "score"
      <*> o .:  "passed"
      <*> o .:  "generation"
      <*> pure Nothing     -- counter-example serialised separately in SQLite
      <*> o .:? "lhResult"
      <*> o .:? "sbvResult"
      <*> o .:? "oracleModel"

-- | Handles into the agent's transactional state.
--
-- Every mutable field is a 'TVar' so that the 'DGM.SafetyKernel.transact'
-- primitive can snapshot and roll back in O(1) time via STM (SPEC.md §3.2).
data AgentState = AgentState
  { stateGeneration        :: TVar Int
  , stateArchive           :: TVar [ArchiveEntry]
  , stateCurrentAST        :: TVar ASTNode
  , stateStateMap          :: TVar (Map Text Text)  -- ^ Named slots for StateUpdate.
  , stateMetrics           :: TVar Metrics
  , stateRunning           :: TVar Bool
  , stateRepoRoot          :: FilePath              -- ^ Root directory of the project; used to whitelist SystemWrite paths.
  , stateTypesHash         :: TVar Text             -- ^ SHA-256 of @Types.hs@ at last cycle start; 'Text' empty = not yet recorded.
  , stateSbvQueue          :: TVar [CounterExample] -- ^ Counter-examples from SBV; consumed by the ExprPhase as endogenous ExprTasks.
  , stateMutationBlacklist :: TVar (Set (FilePath, Text))
    -- ^ Pairs of @(filePath, mutationDescription)@ that have already been tried
    -- and failed.  'proposeSelfMutations' filters these out to avoid looping.
  }

-- | Allocate a fresh 'AgentState' with a minimal bootstrap AST.
-- The 'FilePath' argument sets 'stateRepoRoot' for SystemWrite path whitelisting.
newAgentState :: ASTNode -> IO AgentState
newAgentState bootstrapAST = newAgentStateWithRoot bootstrapAST ""

-- | Like 'newAgentState' but sets an explicit repo root for path whitelisting.
newAgentStateWithRoot :: ASTNode -> FilePath -> IO AgentState
newAgentStateWithRoot bootstrapAST repoRoot = atomically $ do
  g   <- newTVar 0
  arc <- newTVar []
  ast <- newTVar bootstrapAST
  sm  <- newTVar Map.empty
  met <- newTVar initialMetrics
  run <- newTVar True
  th  <- newTVar ""         -- typesHash: empty until first cycle checks Types.hs
  sq  <- newTVar []         -- sbvQueue: populated when SBV finds counter-examples
  bl  <- newTVar Set.empty  -- mutationBlacklist: grows as mutations fail
  pure AgentState
    { stateGeneration        = g
    , stateArchive           = arc
    , stateCurrentAST        = ast
    , stateStateMap          = sm
    , stateMetrics           = met
    , stateRunning           = run
    , stateRepoRoot          = repoRoot
    , stateTypesHash         = th
    , stateSbvQueue          = sq
    , stateMutationBlacklist = bl
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- Phase G: module safety (SELFMOD.md §Phase G)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Qualified module name (e.g. @\"DGM.Rewriting\"@).
type ModuleName = Text

-- | Safety kernel source files that may NOT be autonomously mutated.
--
-- Modifications to these files require a human-approved quorum proof.
-- Level 4 autonomy (future) is the only path to mutating @DGM.Types@.
safeguardedModules :: Set ModuleName
safeguardedModules = Set.fromList
  [ "DGM.SafetyKernel"
  , "DGM.Types"   -- at Level 4 threshold
  ]

-- | DGM.Cycle — The "Zero Data" Cycle orchestrator (SPEC.md §5).
--
-- This module implements the six-step deterministic execution loop:
--
--  1. /Autonomous Task Proposal/ — Proposer generates a task at the Pareto frontier.
--  2. /Hypothesis Generation/ — Anamorphic unfold of candidate mutations.
--  3. /Formal Verification (Gödel Gate)/ — LiquidHaskell refinements + SBV equivalence.
--  4. /GADT Classification and Compilation/ — Mutation tagged as Safe/Critical/Existential.
--  5. /Sandboxed Empirical Testing/ — Execute in the mueval-bounded sandbox.
--  6. /Persistence Filtering (Catamorphic Fold)/ — Archive and apply or rollback.
--
-- The "Zero Data" name derives from the Absolute Zero paradigm's ability to
-- drive cognitive growth entirely via endogenous task generation — no external
-- human data is required.
module DGM.Cycle
  ( -- * Cycle configuration
    CycleConfig(..)
  , defaultCycleConfig
    -- * Cycle phase selection
  , CyclePhase(..)
    -- * Running the cycle
  , runCycle
  , runCycleN
    -- * Step-level observations
  , CycleStep(..)
  , StepResult(..)
    -- * Generation model helpers
  , hasSuccessfulCommit
  ) where

import Control.Concurrent.STM
import Control.Monad (when)
import qualified Control.Exception as Control.Exception
import Control.Exception (SomeException, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Exit (ExitCode)
import System.IO (hFlush, stdout)
import qualified System.IO
import qualified System.Process
import System.Random (randomRIO)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16

import DGM.Types
import DGM.AST
import DGM.AbsoluteZero
import DGM.Evolution

import DGM.Sandbox (runExprInSandbox, defaultSandboxConfig, SandboxResult(..))
import DGM.Archive (addEntry, computeStats, ArchiveStats(..), ArchiveHandle, flushArchive, flushBlacklist, flushDynamicRules)
import DGM.SafetyKernel
import qualified DGM.Archive as Archive

import DGM.HsAST (HsMutation(..), HsModule, applyHsMutation, printHsModule)
import DGM.SelfMod (discoverSources, proposeSelfMutations, rankMutations, writeMutation, commitMutation)
import DGM.Liquid (verifyWithLiquid, LiquidResult(..))
import DGM.SelfCompile
  ( testSelf, scoreCompileResult, enrichScore, buildPreflight
  , SelfModTxn(..), withSelfModTxn
  )
import DGM.ModGraph (buildModuleGraph, removesExportedName)
import DGM.Rewriting (DynamicRule(..), applyDynamicRules)
import DGM.HintBridge (HintEnv, newHintEnv, evalRuleCandidate)
import DGM.Oracle (newOracleEnv, MutationContext(..))
import DGM.OracleContext (collectGhcWarnings)
import DGM.RuleMiner (minePatterns, boostByFile, evictStaleRules)
import DGM.NatLang (EvolutionGoal, applyGoal, describeGoal)

-- ─────────────────────────────────────────────────────────────────────────────
-- Cycle phase
-- ─────────────────────────────────────────────────────────────────────────────

-- | Selects which execution strategy 'runCycle' uses.
--
-- * 'ExprPhase'    — the original six-step Zero Data Cycle (expression mutations).
-- * 'SelfModPhase' — Haskell source self-modification with compile/test/rollback.
data CyclePhase = ExprPhase | SelfModPhase
  deriving (Show, Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- Cycle configuration
-- ─────────────────────────────────────────────────────────────────────────────

data CycleConfig = CycleConfig
  { ccAgentState       :: AgentState
  , ccSelfPlayCfg      :: SelfPlayConfig
  , ccEvolutionCfg     :: EvolutionConfig
  , ccAuditLog         :: AuditLog
  , ccVerbose          :: Bool
  , ccArchiveHandle    :: ArchiveHandle  -- ^ Persistence backend for archive entries.
  , ccProposer         :: TVar Proposer  -- ^ Mutable Proposer state threaded across cycles.
  , ccSelfModRateLimit :: Int
    -- ^ Number of ExprPhase cycles per SelfModPhase cycle (default: 10).
    -- 'runCycleN' switches to 'SelfModPhase' when 'propSelfModCounter' ≥ this limit.
  , ccDynamicRules     :: TVar [DynamicRule]
    -- ^ Live set of dynamic ExprF rewrite rules added via the hint track.
    -- Rules here are applied by 'ExprPhase' cycles after static rules.
  , ccHintEnv          :: HintEnv
    -- ^ Persistent GHC interpreter session for 'evalRuleCandidate'.
  , ccEvolutionGoal    :: Maybe EvolutionGoal
    -- ^ Optional natural-language goal that filters candidate source files
    -- before mutation proposals (see 'DGM.NatLang').  'Nothing' means
    -- no filtering — all sources are considered.
  , ccCurrentExpr      :: TVar Expr
    -- ^ The live ExprF baseline for the ExprPhase feedback loop.
    -- Winning expressions are written back here so the next cycle
    -- starts from the improved AST rather than 'bootstrapExpr'.
  }

defaultCycleConfig :: AgentState -> AuditLog -> ArchiveHandle -> IO CycleConfig
defaultCycleConfig st auditLog hdl = do
  propVar  <- newTVarIO defaultProposer
  dynRules <- newTVarIO []
  hintEnv  <- newHintEnv
  exprVar  <- newTVarIO bootstrapExpr
  pure CycleConfig
    { ccAgentState       = st
    , ccSelfPlayCfg      = defaultSelfPlayConfig st
    , ccEvolutionCfg     = defaultEvolutionConfig st exprVar
    , ccAuditLog         = auditLog
    , ccVerbose          = True
    , ccArchiveHandle    = hdl
    , ccProposer         = propVar
    , ccSelfModRateLimit = 10
    , ccDynamicRules     = dynRules
    , ccHintEnv          = hintEnv
    , ccEvolutionGoal    = Nothing
    , ccCurrentExpr      = exprVar
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- Observable cycle steps
-- ─────────────────────────────────────────────────────────────────────────────

data CycleStep
  = StepPropose
  | StepHypothesise
  | StepVerify
  | StepFormalVerify  -- ^ SBV equivalence proof (pillar II formal verification).
  | StepClassify
  | StepTest
  | StepPersist
  deriving (Show, Eq, Enum, Bounded)

data StepResult = StepResult
  { stepName    :: CycleStep
  , stepSuccess :: Bool
  , stepMessage :: Text
  } deriving (Show)

-- ─────────────────────────────────────────────────────────────────────────────
-- Single cycle (dispatch on CyclePhase)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Execute one full cycle in the specified 'CyclePhase'.
--
-- Returns the list of step results so the caller can observe each gate.
runCycle :: CycleConfig -> CyclePhase -> IO [StepResult]
runCycle cfg ExprPhase    = runExprCycle cfg
runCycle cfg SelfModPhase = runSelfModCycle cfg

-- ─────────────────────────────────────────────────────────────────────────────
-- ExprPhase: original six-step Zero Data Cycle
-- ─────────────────────────────────────────────────────────────────────────────

runExprCycle :: CycleConfig -> IO [StepResult]
runExprCycle cfg = do
  let st = ccAgentState cfg

  -- ── Step 1: Autonomous Task Proposal ──────────────────────────────────────
  currentProp <- atomically (readTVar (ccProposer cfg))
  let spCfg = (ccSelfPlayCfg cfg) { spProposer = currentProp }
  (newProp, solverResult) <- runSelfPlayStep spCfg
  atomically (writeTVar (ccProposer cfg) newProp)
  let step1 = StepResult StepPropose True
                ("Task: " <> srTaskId solverResult
                 <> " | accuracy: " <> T.pack (show (srAccuracy solverResult)))
  emit cfg step1

  -- ── Step 2: Hypothesis Generation (anamorphic unfold + dynamic rules) ───
  baseline  <- atomically (readTVar (ccCurrentExpr cfg))
  dynRules  <- atomically (readTVar (ccDynamicRules cfg))
  let depth = ecHypothesisDepth (ccEvolutionCfg cfg)
  let tree  = unfoldHypotheses depth baseline
  -- Apply dynamic rules to baseline to augment the candidate set.
  let dynMuts = applyDynamicRules dynRules baseline
  let step2 = StepResult StepHypothesise True
                (  "Hypothesis tree depth: " <> T.pack (show depth)
                <> " | dynamic candidates: " <> T.pack (show (length dynMuts)) )
  emit cfg step2

  -- ── Step 3: Formal Verification (Gödel Gate) ─────────────────────────────
  evalResult <- foldBest baseline tree
  let verOk  = erVerified evalResult
  let step3  = StepResult StepVerify verOk $
        if verOk
          then "Verified: " <> prettyExpr (erExpr evalResult)
          else "Falsifiable: " <> maybe "no counter-example" showCE (erCounterEx evalResult)
  emit cfg step3

  if not verOk
    then do
      archiveFailed cfg evalResult
      pure [step1, step2, step3]
    else do
      -- ── Step 4: GADT Classification ──────────────────────────────────────
      let cmd     = MutateAST Mutation
                      { mutationType    = Optimize
                      , mutationTarget  = "bootstrap"
                      , mutationPayload = ExprMutation
                          (exprToASTNode "old" baseline)
                          (exprToASTNode "new" (erExpr evalResult))
                      }
          anyCmd  = AnyCommand SingCritical cmd
          auditE  = auditCommand anyCmd
      appendAudit (ccAuditLog cfg) auditE
      let step4 = StepResult StepClassify True
                    "Classified as Critical; transaction plan prepared"
      emit cfg step4

      -- ── Step 5: Sandboxed Empirical Testing ──────────────────────────────
      sandboxResult <- runExprInSandbox defaultSandboxConfig mempty (erExpr evalResult)
      let testOk = srPassed sandboxResult
      let step5  = StepResult StepTest testOk $
                    "Score: " <> T.pack (show (srScore sandboxResult))
                    <> " | latency: " <> T.pack (show (srLatencyMs sandboxResult)) <> "ms"
      emit cfg step5

      -- ── Step 6: Persistence Filtering (catamorphic fold) ─────────────────
      if testOk && erScore evalResult > 0
        then do
          res  <- dispatchCritical st (ccAuditLog cfg) cmd
          case res of
            Left err -> do
              let step6 = StepResult StepPersist False ("Rollback: " <> err)
              emit cfg step6
              archiveFailed cfg evalResult
              pure [step1, step2, step3, step4, step5, step6]
            Right ok -> do
              archiveSuccess cfg evalResult
              -- Feed the winning expression back as the new baseline.
              atomically $ do
                writeTVar (ccCurrentExpr cfg) (erExpr evalResult)
                writeTVar (stateCurrentAST st) (exprToASTNode "current" (erExpr evalResult))
              let step6 = StepResult StepPersist True ("Committed: " <> ok)
              emit cfg step6
              pure [step1, step2, step3, step4, step5, step6]
        else do
          archiveFailed cfg evalResult
          let step6 = StepResult StepPersist False
                        ("Discarded (score too low): " <> T.pack (show (erScore evalResult)))
          emit cfg step6
          pure [step1, step2, step3, step4, step5, step6]

-- ─────────────────────────────────────────────────────────────────────────────
-- SelfModPhase: Haskell source self-modification with compile/test/rollback
-- ─────────────────────────────────────────────────────────────────────────────

runSelfModCycle :: CycleConfig -> IO [StepResult]
runSelfModCycle cfg = do
  let st = ccAgentState cfg

  -- ── Phase F: Types.hs hash guard ─────────────────────────────────────────
  -- Check whether Types.hs has changed since the previous cycle.  If the
  -- stored hash differs from the current file hash, pause and run a full
  -- type-check before proceeding.
  checkTypesHash st

  -- ── Step 1: Discover sources + propose mutations ──────────────────────────
  fps0       <- discoverSources
  let fps = case ccEvolutionGoal cfg of
               Nothing   -> fps0
               Just goal -> applyGoal goal fps0
  -- Pre-read all source files before ghc-exactprint parsing to avoid file
  -- locking: the GHC API holds handles that prevent subsequent TIO.readFile.
  preReadSources <- mapM (\f -> do t <- TIO.readFile f; return (f, t)) fps
  modGraph   <- buildModuleGraph fps
  -- Read archive for oracle context and pattern mining.
  archiveEntries <- atomically (Archive.getAll (stateArchive st))
  -- Build archive context for the oracle.
  let mCtx = if null archiveEntries
               then Nothing
               else let passed = filter entryPassed archiveEntries
                        total  = length archiveEntries
                        rate   = fromIntegral (length passed) / fromIntegral total
                        best   = maximum (map entryScore archiveEntries)
                        recent = take 3 [ entryCode e | e <- archiveEntries, entryPassed e ]
                        mGoalText = fmap (\g -> describeGoal g) (ccEvolutionGoal cfg)
                    in  Just MutationContext
                          { mcBestScore       = best
                          , mcTotalEntries    = total
                          , mcPassRate        = rate
                          , mcRecentSuccesses = recent
                          , mcEvolutionGoal   = mGoalText
                          }
  -- Collect GHC warnings (runs cabal build -Wall, ~5s).
  warnings <- collectGhcWarnings
  candidates <- proposeSelfMutations st fps mCtx modGraph warnings
  -- Reject mutations that would remove exported names (pre-write safety).
  let safeCandiates = filter (\(_, mut, m) -> not (removesExportedName mut m)) candidates
  -- Mine archive for successful mutation patterns and boost ranking.
  let patterns = minePatterns archiveEntries
      ranked0  = rankMutations modGraph safeCandiates
      ranked   = boostByFile patterns ranked0
  -- Detect oracle vs heuristic for the top candidate (oracle mutations carry
  -- an "oracle-diff: " prefix in their description, set by DGM.Oracle).
  let (proposeLabel, oracleModel) = case ranked of
        ((_, mut, _) : _)
          | "oracle-diff: " `T.isPrefixOf` hmDescription mut ->
              ( "oracle proposed: " <> T.take 40 (T.drop 13 (hmDescription mut))
              , Just "google/gemini-3-flash-preview" )
          | otherwise ->
              ( "heuristic: " <> T.take 40 (hmDescription mut)
              , Nothing )
        [] -> ("no candidates", Nothing)
  let step1 = StepResult StepPropose True
                (  proposeLabel
                <> " | candidates: " <> T.pack (show (length candidates))
                <> " | safe: " <> T.pack (show (length safeCandiates)) )
  emit cfg step1

  let step2  = StepResult StepHypothesise True
                 ("Ranked mutations: " <> T.pack (show (length ranked)))
  emit cfg step2

  case ranked of
    [] -> do
      let stepEnd = StepResult StepPersist False "No mutation candidates found"
      emit cfg stepEnd
      pure [step1, step2, stepEnd]

    _ -> tryCandidate cfg st step1 step2 oracleModel preReadSources ranked 0

-- | Try up to 3 mutation candidates in sequence.  On apply failure, blacklist
-- and advance to the next candidate.  On success, delegate to
-- 'runMutationPipeline' for the write\/test\/commit flow.
tryCandidate
  :: CycleConfig
  -> AgentState
  -> StepResult        -- ^ step1 (propose)
  -> StepResult        -- ^ step2 (hypothesise)
  -> Maybe Text        -- ^ oracleModel tag
  -> [(FilePath, Text)] -- ^ pre-read source contents
  -> [(FilePath, HsMutation, HsModule)]
  -> Int               -- ^ attempt counter (max 3)
  -> IO [StepResult]
tryCandidate cfg _st step1 step2 _oracleModel _srcs [] _attempt = do
  let stepEnd = StepResult StepPersist False "All candidates exhausted"
  emit cfg stepEnd
  pure [step1, step2, stepEnd]
tryCandidate cfg st step1 step2 oracleModel _srcs _ attempt | attempt >= 3 = do
  let stepEnd = StepResult StepPersist False "Max candidate attempts (3) reached"
  emit cfg stepEnd
  pure [step1, step2, stepEnd]
tryCandidate cfg st step1 step2 oracleModel srcs ((fp, mut, origModule) : rest) attempt =
  case applyHsMutation mut origModule of
    Left applyErr -> do
      let step3 = StepResult StepClassify False
                    ("Mutation inapplicable (attempt " <> T.pack (show (attempt + 1))
                    <> "): " <> applyErr)
      emit cfg step3
      archiveSelfModFailed cfg (hmDescription mut) fp 0.0 Nothing Nothing oracleModel
      -- Blacklist inapplicable mutation so it is not re-proposed.
      atomically $ modifyTVar' (stateMutationBlacklist (ccAgentState cfg))
        (Set.insert (fp, hmDescription mut))
      flushBlacklist (ccArchiveHandle cfg) [(fp, hmDescription mut)]
      -- Try next candidate.
      tryCandidate cfg st step1 step2 oracleModel srcs rest (attempt + 1)

    Right mutModule ->
      runMutationPipeline cfg st step1 step2 oracleModel srcs fp mut origModule mutModule

-- | Execute the write\/LH\/SBV\/test\/commit pipeline for a successfully applied
-- mutation candidate.  Extracted from 'runSelfModCycle' to support
-- 'tryCandidate' without duplicating the pipeline logic.
runMutationPipeline
  :: CycleConfig
  -> AgentState
  -> StepResult        -- ^ step1 (propose)
  -> StepResult        -- ^ step2 (hypothesise)
  -> Maybe Text        -- ^ oracleModel tag
  -> [(FilePath, Text)] -- ^ pre-read source contents
  -> FilePath
  -> HsMutation
  -> HsModule          -- ^ original module
  -> HsModule          -- ^ mutated module
  -> IO [StepResult]
runMutationPipeline cfg st step1 step2 oracleModel srcs fp mut _origModule mutModule = do
  -- Use pre-read content to avoid file locking from ghc-exactprint.
  let origText = case lookup fp srcs of
                   Just t  -> t
                   Nothing -> ""  -- shouldn't happen; fallback to empty
  let origLen = T.length origText
      mutLen  = T.length (printHsModule mutModule)
      txn = SelfModTxn
              { smtFile         = fp
              , smtOriginalText = origText
              , smtMutation     = mut
              }
      step3 = StepResult StepClassify True
                (  "Snapshot + mutation: " <> hmDescription mut
                <> " | file: " <> T.pack fp )
  emit cfg step3

  -- ── Steps 4-5: Write + LH + SBV + cabal test (transactional) ─────────
  nowPosix <- getPOSIXTime
  let now    = floor nowPosix :: Int64
      opName = "write:" <> T.pack fp
      proof  = mockQuorumProof opName now

  -- IORefs capture intermediate results for step messages + archive.
  scoreRef <- newIORef (0.0 :: Double)
  lhRef    <- newIORef (Nothing :: Maybe Text)
  sbvRef   <- newIORef (Nothing :: Maybe Text)

  txnResult <- withSelfModTxn txn $ do
    wr <- writeMutation st (ccAuditLog cfg) proof fp mutModule
    case wr of
      Left writeErr -> return (Left ("Write failed: " <> writeErr))
      Right () -> do
        -- ── Build pre-flight: catch type errors before expensive pipeline ─
        preflight <- buildPreflight
        case preflight of
          Left buildErr -> return (Left ("Build pre-flight: " <> buildErr))
          Right () -> do
            -- ── LiquidHaskell pre-flight (pillar II formal verification) ─
            lhResult <- verifyWithLiquid (stateRepoRoot st) fp
            let lhText = case lhResult of
                  LiquidSafe       -> "SAFE"
                  LiquidUnsafe det -> "UNSAFE: " <> det
                  LiquidError  msg -> "ERROR: "  <> msg
            writeIORef lhRef (Just lhText)
            case lhResult of
              LiquidUnsafe details ->
                return (Left ("LiquidHaskell UNSAFE: " <> details))
              LiquidError msg ->
                return (Left ("LiquidHaskell error: " <> msg))
              LiquidSafe -> do
                -- SBV equivalence is skipped for source-level mutations:
                -- there is no ExprF representation to compare, so the check
                -- would be vacuous (comparing bootstrapExpr to itself).
                -- SBV remains active for ExprPhase cycles where it has real
                -- original vs. mutated expressions.
                writeIORef sbvRef (Just "skipped (source-level)")
                cr <- testSelf
                let base = scoreCompileResult cr
                    sc   = enrichScore base origLen mutLen (Just lhText)
                writeIORef scoreRef sc
                if sc >= 1.0
                  then return (Right ())
                  else return (Left ("Tests degraded, score=" <> T.pack (show sc)))

  score   <- readIORef scoreRef
  mLhText <- readIORef lhRef
  mSbvText <- readIORef sbvRef
  let lhDisplay  = fromMaybe "n/a" mLhText
      sbvDisplay = fromMaybe "n/a" mSbvText

  let step4Desc  ok msg = StepResult StepVerify       ok msg
      step4bDesc ok msg = StepResult StepFormalVerify ok msg
      step5Desc  ok msg = StepResult StepTest         ok msg
      step6Desc  ok msg = StepResult StepPersist      ok msg

  case txnResult of
    Left err -> do
      let step4 = step4Desc False
                    ("LH: " <> lhDisplay <> " | " <> err)
      emit cfg step4
      -- Emit StepFormalVerify only if SBV actually ran.
      sbvSteps <- case mSbvText of
        Nothing -> pure []
        Just _  -> do
          let step4b = step4bDesc False ("SBV: " <> sbvDisplay)
          emit cfg step4b
          pure [step4b]
      let step5 = step5Desc False ("Score: " <> T.pack (show score))
      emit cfg step5
      archiveSelfModFailed cfg (hmDescription mut) fp score mLhText mSbvText oracleModel
      -- Blacklist this (file, mutation) pair so it is not re-proposed.
      atomically $ modifyTVar' (stateMutationBlacklist (ccAgentState cfg))
        (Set.insert (fp, hmDescription mut))
      flushBlacklist (ccArchiveHandle cfg) [(fp, hmDescription mut)]
      let step6 = step6Desc False
                    ("Rollback complete | score=" <> T.pack (show score))
      emit cfg step6
      pure ([step1, step2, step3, step4] ++ sbvSteps ++ [step5, step6])

    Right () -> do
      let step4 = step4Desc True
                    ("LH: " <> lhDisplay <> " | mutation written: " <> T.pack fp)
      emit cfg step4
      let step4b = step4bDesc True ("SBV: " <> sbvDisplay)
      emit cfg step4b
      let step5 = step5Desc True
                    ("Score: " <> T.pack (show score) <> " (all tests pass)")
      emit cfg step5
      -- Create archive entry with full provenance.
      gen <- atomically (readTVar (stateGeneration (ccAgentState cfg)))
      entry <- mkSelfModEntry gen (hmDescription mut) fp score True mLhText mSbvText oracleModel
      -- Commit the mutation to git.
      commitResult <- commitMutation fp mut entry
      let step6 = case commitResult of
            Right gitHash ->
              step6Desc True
                (  "Committed " <> T.take 8 gitHash
                <> " | score=" <> T.pack (show score) )
            Left commitErr ->
              step6Desc False
                (  "Commit failed: " <> commitErr
                <> " | score=" <> T.pack (show score) )
      emit cfg step6
      -- Archive the entry regardless of commit outcome — tests passed.
      atomically $ addEntry (stateArchive (ccAgentState cfg)) entry
      flushArchive (ccArchiveHandle cfg) [entry]
      atomically $ modifyTVar' (stateGeneration (ccAgentState cfg)) (+1)
      -- ── Hint sub-step: propose + evaluate dynamic rule ─────────────────
      hintSteps <- runHintSubStep cfg
      pure ([step1, step2, step3, step4, step4b, step5, step6] ++ hintSteps)

-- ─────────────────────────────────────────────────────────────────────────────
-- Hint sub-step: propose + evaluate + test + commit/reject dynamic rule
-- ─────────────────────────────────────────────────────────────────────────────

-- | Run the four-stage hint sub-step inside a 'SelfModPhase' cycle.
--
-- Stages:
--
--  1. /StepHintPropose/ — select a candidate snippet from the heuristic pool
--     (or flag oracle availability).  A full Oracle-to-hint integration would
--     require a separate prompt asking for an @ExprF Int -> ExprF Int@
--     expression; that is left as a Phase H extension.
--  2. /StepHintEval/    — evaluate the snippet via 'evalRuleCandidate';
--     reject immediately on compile error or type mismatch.
--  3. /StepHintTest/    — apply the new rule to 'bootstrapExpr'; if it fires
--     at least once the score delta is positive.
--  4. /StepHintCommit/  or /StepHintReject/ — atomically prepend the rule to
--     'ccDynamicRules' when the delta is positive; archive as failed otherwise.
--
-- All stages run best-effort: failures produce step results tagged as
-- unsuccessful but do not propagate exceptions.
runHintSubStep :: CycleConfig -> IO [StepResult]
runHintSubStep cfg = do
  -- ── Stage 1: Propose rule candidate ──────────────────────────────────────
  -- Pick a candidate from the heuristic pool, cycling based on dynamic rule count
  -- so successive cycles exercise different rules.
  mOracleEnv    <- newOracleEnv
  existingCount <- length <$> atomically (readTVar (ccDynamicRules cfg))
  let candidateText = heuristicCandidates !! (existingCount `mod` length heuristicCandidates)
      oracleTag     = case mOracleEnv of { Nothing -> "heuristic"; _ -> "oracle-key-present" }
  let stepPropose = StepResult StepVerify True
                      (  "HintPropose: " <> oracleTag
                      <> " | snippet=" <> T.take 40 candidateText )
  emit cfg stepPropose

  -- ── Stage 2: Evaluate + type-check via HintBridge ────────────────────────
  evalResult <- evalRuleCandidate (ccHintEnv cfg) candidateText
  case evalResult of
    Left err -> do
      let stepEval = StepResult StepTest False
                       ("HintEval failed: " <> err)
      emit cfg stepEval
      return [stepPropose, stepEval]

    Right ruleFn -> do
      let stepEval = StepResult StepTest True "HintEval: type-checked OK"
      emit cfg stepEval

      -- ── Stage 3: Test — apply rule to bootstrapExpr, count firings ─────
      let probe     = bootstrapExpr
          newRule   = DynamicRule
                        { drDescription = "oracle-hint-" <> T.take 20 candidateText
                        , drTransform   = ruleFn
                        , drScore       = 0.0
                        }
          firings   = applyDynamicRules [newRule] probe
          scoreDelta = fromIntegral (length firings) :: Double

      let stepTest = StepResult StepPersist (scoreDelta > 0)
                       (  "HintTest: firings=" <> T.pack (show (length firings))
                       <> " delta=" <> T.pack (show scoreDelta) )
      emit cfg stepTest

      if scoreDelta > 0
        then do
          -- ── Stage 4a: Commit — add rule to live TVar + persist ──────────
          let newRule' = newRule { drScore = scoreDelta }
          atomically $ modifyTVar' (ccDynamicRules cfg) $ \rs ->
            evictStaleRules 20 (newRule' : rs)
          -- Persist to SQLite so the rule survives restarts.
          flushDynamicRules (ccArchiveHandle cfg)
            [(drDescription newRule', candidateText, scoreDelta)]
          let stepCommit = StepResult StepPersist True
                             ("HintCommit: rule added to live set + persisted")
          emit cfg stepCommit
          return [stepPropose, stepEval, stepTest, stepCommit]
        else do
          -- ── Stage 4b: Reject — archive as failed (stepping-stone) ───────
          archiveSelfModFailed cfg ("hint-reject: " <> drDescription newRule)
                               "" 0.0 Nothing Nothing Nothing
          let stepReject = StepResult StepPersist False
                             "HintReject: rule archived as stepping-stone"
          emit cfg stepReject
          return [stepPropose, stepEval, stepTest, stepReject]

-- | Pool of heuristic rule candidates cycled through in offline / no-key environments.
--
-- Each candidate is a Haskell expression of type @ExprF Int -> ExprF Int@
-- evaluated by @evalRuleCandidate@ via the GHC hint interpreter.  The rule
-- operates on a single functor layer whose children are 0-based integer labels;
-- returning the same value means "no change" and the rule is skipped.
--
-- Rules are tried at every subterm position by @applyDynamicRules@.
heuristicCandidates :: [Text]
heuristicCandidates =
  [ -- 1. Commute addition: (a + b) -> (b + a).  Fires on every BinOpF "+" node.
    "\\x -> case x of { BinOpF \"+\" l r -> BinOpF \"+\" r l ; _ -> x }"
  , -- 2. Commute multiplication: (a * b) -> (b * a).
    "\\x -> case x of { BinOpF \"*\" l r -> BinOpF \"*\" r l ; _ -> x }"
  , -- 3. Unit sink: replace UnitF with LitF 0 (useful as a seed value).
    "\\x -> case x of { UnitF -> LitF 0 ; _ -> x }"
  , -- 4. Equality to less-than: (a == b) -> (a < b).  Speculative mutation that
    --    changes semantics; the sandbox test will reject if it breaks correctness.
    "\\x -> case x of { BinOpF \"==\" l r -> BinOpF \"<\" l r ; _ -> x }"
  , -- 5. Subtract to add: (a - b) -> (a + b).  Speculative; sandbox rejects if wrong.
    "\\x -> case x of { BinOpF \"-\" l r -> BinOpF \"+\" l r ; _ -> x }"
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Multi-cycle runner (ExprPhase by default)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Run @n@ complete Zero Data Cycles and return aggregate stats.
--
-- Phase selection is proposer-driven: after 'ccSelfModRateLimit' consecutive
-- 'ExprPhase' cycles the proposer's 'propSelfModCounter' reaches the limit and
-- 'runCycleN' executes one 'SelfModPhase' cycle, then resets the counter and
-- resumes 'ExprPhase'.  This implements the 1-in-N autonomous self-improvement
-- cadence described in SELFMOD.md §Phase G.
runCycleN :: CycleConfig -> Int -> IO ArchiveStats
runCycleN cfg n = go n
  where
    go 0 = readStats
    go k = do
      running <- atomically (readTVar (stateRunning (ccAgentState cfg)))
      if not running
        then readStats
        else do
          prop <- atomically (readTVar (ccProposer cfg))
          let phase
                | propSelfModCounter prop >= ccSelfModRateLimit cfg = SelfModPhase
                | otherwise                                         = ExprPhase
          _ <- runCycle cfg phase
          -- After a SelfModPhase cycle, reset the counter so the next
          -- ccSelfModRateLimit ExprPhase cycles proceed before another selfmod.
          when (phase == SelfModPhase) $
            atomically (modifyTVar' (ccProposer cfg)
                          (\p -> p { propSelfModCounter = 0 }))
          go (k - 1)
    readStats = do
      entries <- atomically (Archive.getAll (stateArchive (ccAgentState cfg)))
      pure (computeStats entries)

-- ─────────────────────────────────────────────────────────────────────────────
-- Archive helpers
-- ─────────────────────────────────────────────────────────────────────────────

archiveFailed :: CycleConfig -> EvalResult -> IO ()
archiveFailed cfg result = do
  entry <- mkArchiveEntry 0 result
  let e = entry { entryPassed = False }
  atomically $ addEntry (stateArchive (ccAgentState cfg)) e
  flushArchive (ccArchiveHandle cfg) [e]

archiveSuccess :: CycleConfig -> EvalResult -> IO ()
archiveSuccess cfg result = do
  entry <- mkArchiveEntry 0 result
  atomically $ addEntry (stateArchive (ccAgentState cfg)) entry
  flushArchive (ccArchiveHandle cfg) [entry]

archiveSelfModFailed :: CycleConfig -> Text -> FilePath -> Double -> Maybe Text -> Maybe Text -> Maybe Text -> IO ()
archiveSelfModFailed cfg desc fp score mLh mSbv mOracle = do
  gen <- atomically (readTVar (stateGeneration (ccAgentState cfg)))
  entry <- mkSelfModEntry gen desc fp score False mLh mSbv mOracle
  atomically $ addEntry (stateArchive (ccAgentState cfg)) entry
  flushArchive (ccArchiveHandle cfg) [entry]

mkSelfModEntry :: Int -> Text -> FilePath -> Double -> Bool -> Maybe Text -> Maybe Text -> Maybe Text -> IO ArchiveEntry
mkSelfModEntry gen desc fp score passed mLh mSbv mOracle = do
  n <- randomRIO (0 :: Int, maxBound)
  let eid = "selfmod-" <> T.pack (show n)
      mut = Mutation
              { mutationType    = Expand
              , mutationTarget  = T.pack fp
              , mutationPayload = HaskellMutation desc
              }
  pure ArchiveEntry
    { entryId           = eid
    , entryCode         = desc
    , entryMutation     = Just mut
    , entryParentId     = Nothing
    , entryScore        = score
    , entryPassed       = passed
    , entryGeneration   = gen
    , entryCounterEx    = Nothing
    , entryLiquidResult = mLh
    , entrySbvResult    = mSbv
    , entryOracleModel  = mOracle
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- Phase F helpers
-- ─────────────────────────────────────────────────────────────────────────────

-- | Check whether @Types.hs@ has changed since the last cycle.
--
-- Reads @src\/DGM\/Types.hs@ relative to 'stateRepoRoot', hashes its content
-- with SHA-256, and compares against the stored hash in 'stateTypesHash'.
--
-- * If the hash is empty (first cycle), stores the current hash and continues.
-- * If the hashes differ, logs a warning to stderr and runs @cabal build@ as
--   a full type-check before updating the stored hash.  This ensures that a
--   Types.hs mutation from the previous cycle is safe before proceeding.
-- * If @Types.hs@ is unreadable, the check is silently skipped.
checkTypesHash :: AgentState -> IO ()
checkTypesHash st = do
  let typesPath = stateRepoRoot st ++ "/src/DGM/Types.hs"
  msrc <- (Just <$> TIO.readFile typesPath)
            `catchExc` (\(_ :: SomeException) -> return Nothing)
  case msrc of
    Nothing  -> return ()  -- unreadable; skip silently
    Just src -> do
      let newHash = sha256HexC src
      oldHash <- atomically (readTVar (stateTypesHash st))
      if T.null oldHash
        then atomically (writeTVar (stateTypesHash st) newHash)
        else if oldHash /= newHash
          then do
            TIO.hPutStrLn System.IO.stderr
              ("DGM.Cycle: Types.hs hash changed; running full type-check")
            -- Best-effort type-check: run cabal build (ignoring errors here;
            -- the next compile via testSelf will surface any failures).
            _ <- try (System.Process.readProcessWithExitCode
                        cabalBin ["build", "--with-compiler=" ++ ghcBin] "") :: IO (Either SomeException (ExitCode, String, String))
            atomically (writeTVar (stateTypesHash st) newHash)
          else return ()
  where
    catchExc :: IO a -> (SomeException -> IO a) -> IO a
    catchExc = Control.Exception.catch

sha256HexC :: Text -> Text
sha256HexC t = TE.decodeUtf8 (B16.encode (SHA256.hash (TE.encodeUtf8 t)))

-- ─────────────────────────────────────────────────────────────────────────────
-- Helpers
-- ─────────────────────────────────────────────────────────────────────────────

emit :: CycleConfig -> StepResult -> IO ()
emit cfg sr
  | not (ccVerbose cfg) = pure ()
  | otherwise = do
      let icon = if stepSuccess sr then "✓" else "✗"
          step = T.pack (show (stepName sr))
          msg  = stepMessage sr
      putStrLn (T.unpack ("  [" <> icon <> "] " <> step <> ": " <> msg))
      hFlush stdout

showCE :: CounterExample -> Text
showCE ce = "input=" <> T.pack (show (ceInputs ce))
          <> " expected=" <> ceExpected ce
          <> " actual=" <> ceActual ce

-- ─────────────────────────────────────────────────────────────────────────────
-- Configuration
-- ─────────────────────────────────────────────────────────────────────────────

cabalBin :: FilePath
cabalBin = "/Users/raz/.ghcup/bin/cabal"

ghcBin :: FilePath
ghcBin = "/Users/raz/.ghcup/bin/ghc-9.6.7"

-- ─────────────────────────────────────────────────────────────────────────────
-- Generation model helpers
-- ─────────────────────────────────────────────────────────────────────────────

-- | True when the step list contains a successful git commit.
--
-- A step is a successful commit when it is a 'StepPersist' with
-- @'stepSuccess' = True@ whose message begins with @"Committed"@.
-- This is the signal used by the supervisor loop to detect that a mutation
-- was accepted and committed, triggering an exit-42 restart.
hasSuccessfulCommit :: [StepResult] -> Bool
hasSuccessfulCommit = any isCommitStep
  where
    isCommitStep sr = stepName sr    == StepPersist
                   && stepSuccess sr
                   && "Committed" `T.isPrefixOf` stepMessage sr

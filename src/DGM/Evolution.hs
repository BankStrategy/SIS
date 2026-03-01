-- | DGM.Evolution — Category-theoretic evolutionary control loop (SPEC.md §4).
--
-- The self-improvement loop is factored into three morphisms:
--
--   * /Anamorphism/ (@ana@): unfolds a "seed" problem into a tree of candidate
--     AST mutations (the hypothesis space).
--
--   * /Catamorphism/ (@cata@): folds the hypothesis tree, applying the
--     verification gate and empirical test at each node, collapsing to the
--     single best surviving candidate.
--
--   * /Hylomorphism/ (@hylo@): the composition @cata . ana@.  GHC's stream
--     fusion pass eliminates the intermediate tree, so the full hypothesis
--     space is explored without ever being materialised in heap memory
--     (deforestation, SPEC.md §4.2).
--
-- The module also houses the Darwin Gödel Machine's outer evolution loop
-- that repeatedly runs the hylomorphism and commits winning mutations to the
-- archive.
module DGM.Evolution
  ( -- * Hypothesis tree
    HypothesisF(..)
  , Hypothesis
    -- * Morphisms
  , unfoldHypotheses
  , foldBest
  , evolveStep
    -- * Outer loop
  , EvolutionConfig(..)
  , defaultEvolutionConfig
  , runEvolutionLoop
    -- * Scoring
  , EvalResult(..)
  , scoreHypothesis
    -- * Archive helpers
  , mkArchiveEntry
  ) where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomRIO)
import DGM.AST
import DGM.Types
import DGM.Rewriting (generateMutations, defaultRules)
import DGM.Verification (runVerification, defaultVerifConfig)
import DGM.Archive (addEntry)

-- ─────────────────────────────────────────────────────────────────────────────
-- Hypothesis tree base functor
-- ─────────────────────────────────────────────────────────────────────────────

-- | The base functor for the hypothesis search tree.
--
-- Each node is either a leaf (no more mutations applicable) or a branch
-- containing the current expression, the mutation that produced it, and a
-- list of child hypotheses.
data HypothesisF a
  = LeafH  Expr                       -- ^ Terminal: no further mutations.
  | BranchH Expr Mutation [a]         -- ^ Non-terminal: expression, mutation, children.
  deriving (Functor, Foldable, Traversable, Show)

type Hypothesis = Fix HypothesisF

-- ─────────────────────────────────────────────────────────────────────────────
-- Anamorphism: unfold the hypothesis space
-- ─────────────────────────────────────────────────────────────────────────────

-- | Seed type for the anamorphism.
data HypothesisSeed = HypothesisSeed
  { hsExpr  :: Expr
  , hsDepth :: Int   -- ^ Remaining depth budget (termination guarantee).
  }

-- | Unfold a hypothesis tree from a seed expression.
--
-- This is the /anamorphism/ described in SPEC.md §4.1.  The coalgebra
-- generates child seeds for each mutation the rewriting engine proposes,
-- bounded by @hsDepth@ to guarantee termination (mirroring REST's online
-- termination checking).
unfoldHypotheses :: Int -> Expr -> Hypothesis
unfoldHypotheses maxDepth = ana coalg . flip HypothesisSeed maxDepth
  where
    coalg :: HypothesisSeed -> HypothesisF HypothesisSeed
    coalg (HypothesisSeed expr depth)
      | depth <= 0 = LeafH expr
      | otherwise  =
          let mutations = generateMutations defaultRules expr
          in  case mutations of
                [] -> LeafH expr
                ms -> BranchH expr (snd (head ms))
                        [ HypothesisSeed e' (depth - 1) | (e', _) <- ms ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Evaluation result
-- ─────────────────────────────────────────────────────────────────────────────

-- | The result of evaluating a single hypothesis.
data EvalResult = EvalResult
  { erExpr         :: Expr
  , erMutation     :: Maybe Mutation
  , erScore        :: Double
  , erVerified     :: Bool
  , erCounterEx    :: Maybe CounterExample
  } deriving (Show)

-- | Score a hypothesis by running its expression through the verification gate
-- and a simple fitness evaluation.
scoreHypothesis :: Expr -> Expr -> Maybe Mutation -> IO EvalResult
scoreHypothesis baseline candidate mutation = do
  vResult <- runVerification defaultVerifConfig baseline candidate
  let (verified, mce) = case vResult of
        Verified _    -> (True,  Nothing)
        Falsifiable c -> (False, Just c)
        VTimeout _    -> (False, Nothing)
  let score
        | not verified = 0.0
        | otherwise    =
            let nodeReduction =
                  fromIntegral (countNodes baseline - countNodes candidate)
                  / fromIntegral (max 1 (countNodes baseline))
            in  max 0 (0.5 + 0.5 * nodeReduction)
  pure EvalResult
    { erExpr      = candidate
    , erMutation  = mutation
    , erScore     = score
    , erVerified  = verified
    , erCounterEx = mce
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- Catamorphism: fold the best candidate
-- ─────────────────────────────────────────────────────────────────────────────

-- | Fold the hypothesis tree to the single best @EvalResult@.
--
-- This is the /catamorphism/ of SPEC.md §4.1.  It is composed with the
-- anamorphism via 'evolveStep' to form the complete hylomorphism.
--
-- Because we need IO to run verification, we use a monadic fold.
foldBest :: Expr -> Hypothesis -> IO EvalResult
foldBest baseline = foldIO baseline
  where
    -- We cannot use @cata@ directly here because the algebra is monadic.
    -- Instead we hand-write the monadic catamorphism.
    foldIO :: Expr -> Hypothesis -> IO EvalResult
    foldIO base tree =
      case unFix tree of
        LeafH expr ->
          scoreHypothesis base expr Nothing
        BranchH expr mut children -> do
          selfScore   <- scoreHypothesis base expr (Just mut)
          childScores <- mapM (foldIO base) children
          let all'    = selfScore : childScores
              best    = maximumBy (comparing erScore) all'
          pure best

-- ─────────────────────────────────────────────────────────────────────────────
-- Hylomorphism: one evolution step
-- ─────────────────────────────────────────────────────────────────────────────

-- | A single evolution step: unfold → score → fold → best result.
--
-- This is the hylomorphism of SPEC.md §4.1.  In practice GHC's optimizer
-- eliminates the intermediate @Hypothesis@ tree via stream fusion.
evolveStep :: EvolutionConfig -> Expr -> IO EvalResult
evolveStep cfg baseline = do
  let tree = unfoldHypotheses (ecHypothesisDepth cfg) baseline
  foldBest baseline tree

-- ─────────────────────────────────────────────────────────────────────────────
-- Outer evolution loop
-- ─────────────────────────────────────────────────────────────────────────────

data EvolutionConfig = EvolutionConfig
  { ecMaxGenerations   :: Int
  , ecHypothesisDepth  :: Int
  , ecArchiveVar        :: TVar [ArchiveEntry]
  , ecStateVar          :: AgentState
  , ecCurrentExpr       :: TVar Expr
    -- ^ Live ExprF baseline — written back when a verified improvement is found.
  }

defaultEvolutionConfig :: AgentState -> TVar Expr -> EvolutionConfig
defaultEvolutionConfig st exprVar = EvolutionConfig
  { ecMaxGenerations  = 100
  , ecHypothesisDepth = 3
  , ecArchiveVar       = stateArchive st
  , ecStateVar         = st
  , ecCurrentExpr      = exprVar
  }

-- | Run the full DGM evolutionary loop for up to @ecMaxGenerations@ steps.
--
-- Each iteration:
--  1. Reads the current best AST from the archive (or bootstrap).
--  2. Runs 'evolveStep' (the hylomorphism).
--  3. If the result improves on the current best, commits it to state + archive.
--  4. Records the result (pass or fail) in the archive regardless.
runEvolutionLoop :: EvolutionConfig -> IO ()
runEvolutionLoop cfg = do
  let _st = ecStateVar cfg
  loop 0
  where
    loop gen
      | gen >= ecMaxGenerations cfg = pure ()
      | otherwise = do
          running <- atomically (readTVar (stateRunning (ecStateVar cfg)))
          if not running
            then pure ()
            else do
              baseline <- atomically (readTVar (ecCurrentExpr cfg))

              result <- evolveStep cfg baseline

              -- Commit to archive (including failures — SPEC.md §1.1).
              entry <- mkArchiveEntry gen result
              atomically $ do
                addEntry (ecArchiveVar cfg) entry
                modifyTVar' (stateGeneration (ecStateVar cfg)) (+1)
                -- Update metrics
                modifyTVar' (stateMetrics (ecStateVar cfg)) $ \m ->
                  m { totalIterations = totalIterations m + 1
                    , successfulMutations =
                        if erVerified result
                        then successfulMutations m + 1
                        else successfulMutations m
                    , failedMutations =
                        if not (erVerified result)
                        then failedMutations m + 1
                        else failedMutations m
                    }

              -- Write back the winning expression when verified.
              when (erVerified result) $
                atomically $ writeTVar (ecCurrentExpr cfg) (erExpr result)

              loop (gen + 1)

-- | Build an archive entry from an evaluation result.
mkArchiveEntry :: Int -> EvalResult -> IO ArchiveEntry
mkArchiveEntry gen result = do
  eid <- generateId
  pure ArchiveEntry
    { entryId           = eid
    , entryCode         = prettyExpr (erExpr result)
    , entryMutation     = erMutation result
    , entryParentId     = Nothing   -- Full system would track parent IDs.
    , entryScore        = erScore result
    , entryPassed       = erVerified result
    , entryGeneration   = gen
    , entryCounterEx    = erCounterEx result
    , entryLiquidResult = Nothing
    , entrySbvResult    = Nothing
    , entryOracleModel  = Nothing
    }

-- | Generate a simple unique ID (UUID-lite).
generateId :: IO Text
generateId = do
  n <- randomRIO (0 :: Int, maxBound)
  pure ("entry-" <> T.pack (show n))

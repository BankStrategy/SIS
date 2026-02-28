-- | Comprehensive capability demonstration for the Darwin Gödel Machine MVP.
--
-- Walks through every major subsystem with annotated output, showing:
--   1. AST construction, pretty-printing, and morphisms
--   2. Rewriting engine — rule-by-rule and exhaustive simplification
--   3. Equivalence verification — passing and falsifiable cases
--   4. Absolute Zero self-play — proposer, solver, learnability curve
--   5. Safety Kernel — Safe/Critical/Existential GADT dispatch + STM rollback
--   6. Full evolution loop — 8 cycles with archive statistics
module Main where

import Control.Concurrent.STM
import Data.Int (Int64)
import Data.IORef
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Map.Strict as Map
import Data.List (intercalate)

import DGM
import DGM.AST
import DGM.Rewriting
import DGM.Verification
import DGM.AbsoluteZero
import DGM.SafetyKernel
import DGM.Archive
import DGM.Evolution
import DGM.Types

-- ─────────────────────────────────────────────────────────────────────────────
-- Formatting helpers
-- ─────────────────────────────────────────────────────────────────────────────

section :: String -> IO ()
section title = do
  putStrLn ""
  putStrLn $ replicate 60 '═'
  putStrLn $ "  " <> title
  putStrLn $ replicate 60 '═'

sub :: String -> IO ()
sub title = do
  putStrLn ""
  putStrLn $ "  ── " <> title <> " " <> replicate (50 - length title) '─'

note :: String -> IO ()
note msg = putStrLn $ "  │  " <> msg

result :: String -> IO ()
result msg = putStrLn $ "  ▶  " <> msg

pass :: String -> IO ()
pass msg = putStrLn $ "  ✓  " <> msg

fail' :: String -> IO ()
fail' msg = putStrLn $ "  ✗  " <> msg

-- ─────────────────────────────────────────────────────────────────────────────
-- Main
-- ─────────────────────────────────────────────────────────────────────────────

main :: IO ()
main = do
  putStrLn ""
  putStrLn "╔══════════════════════════════════════════════════════════╗"
  putStrLn "║   Darwin Gödel Machine — Capability Demonstration       ║"
  putStrLn "║   Synthesising DGM + Absolute Zero in Haskell           ║"
  putStrLn "╚══════════════════════════════════════════════════════════╝"

  demoAST
  demoRewriting
  demoVerification
  demoAbsoluteZero
  demoSafetyKernel
  demoEvolutionLoop

  putStrLn ""
  putStrLn $ replicate 60 '═'
  putStrLn "  Demonstration complete."
  putStrLn $ replicate 60 '═'
  putStrLn ""

-- ─────────────────────────────────────────────────────────────────────────────
-- 1. AST
-- ─────────────────────────────────────────────────────────────────────────────

demoAST :: IO ()
demoAST = do
  section "1. AST — Fixed-Point Types and Recursion-Scheme Morphisms"

  sub "1a. Bootstrap expression (the agent's initial self)"
  note "bootstrapExpr is a recursive factorial, represented as Fix ExprF:"
  result $ show (prettyExpr bootstrapExpr)

  sub "1b. Catamorphism — countNodes (fold over the tree)"
  note $ "Total AST nodes in bootstrapExpr: " <> show (countNodes bootstrapExpr)

  sub "1c. Catamorphism — collectVars (free variable set)"
  let fvs = collectVars bootstrapExpr
  note $ "Free variables (should be empty — closed expression): " <> show fvs

  sub "1d. Pure interpreter — evalExpr with recursive let"
  let result1 = evalExpr Map.empty bootstrapExpr
  case result1 of
    Right v -> pass $ "evalExpr Map.empty bootstrapExpr = " <> show v
    Left  e -> fail' $ "Unexpected error: " <> show e

  sub "1e. Paramorphism — substituteVar"
  let e1 = binop "+" (var "x") (lit 3)
  let e2 = substituteVar "x" (lit 10) e1
  note $ "Original:     " <> show (prettyExpr e1)
  note $ "After x := 10: " <> show (prettyExpr e2)
  let result2 = evalExpr Map.empty e2
  pass $ "Evaluates to: " <> show result2

  sub "1f. Hylomorphism — build then immediately count via hylo"
  -- Ana (coalg): unfold Int n into ExprF n  → builds n + (n-1) + … + 0
  -- Cata (alg):  fold  ExprF Int → count nodes
  let nodeCount :: ExprF Int -> Int
      nodeCount x = 1 + sum x
      buildExpr :: Int -> ExprF Int
      buildExpr 0 = LitF 0                 -- base case: leaf node, no children
      buildExpr n = BinOpF "+" 0 (n-1)    -- left seed=0 (leaf), right seed recurses
      -- hylo never materialises the intermediate tree (stream fusion)
      hyloResult = hylo nodeCount buildExpr (5 :: Int)
  note "  Ana: n → BinOpF \"+\" 0 (n-1)  (builds 0+(0+(0+(0+(0+0)))))"
  note "  Cata: count all nodes"
  pass $ "hylo nodeCount buildExpr 5 = " <> show hyloResult
       <> "  (6 lits + 5 binops = 11 nodes)"

-- ─────────────────────────────────────────────────────────────────────────────
-- 2. Rewriting
-- ─────────────────────────────────────────────────────────────────────────────

demoRewriting :: IO ()
demoRewriting = do
  section "2. Rewriting Engine — REST-Style Term Simplification"

  sub "2a. Constant folding: (3 + 4) → 7"
  let e = binop "+" (lit 3) (lit 4)
  note $ "Input:  " <> show (prettyExpr e)
  let (r, steps, _) = rewrite defaultConfig defaultRules e
  pass $ "Output: " <> show (prettyExpr r) <> "  (" <> show steps <> " step)"

  sub "2b. Identity elimination: (x + 0) → x"
  let e2 = binop "+" (var "x") (lit 0)
  note $ "Input:  " <> show (prettyExpr e2)
  let (r2, s2, _) = rewrite defaultConfig defaultRules e2
  pass $ "Output: " <> show (prettyExpr r2) <> "  (" <> show s2 <> " step)"

  sub "2c. Chained simplification: (2 * (3 + 4)) → 14"
  let e3 = binop "*" (lit 2) (binop "+" (lit 3) (lit 4))
  note $ "Input:  " <> show (prettyExpr e3)
  let (r3, s3, c3) = rewrite defaultConfig defaultRules e3
  pass $ "Output: " <> show (prettyExpr r3)
        <> "  (" <> show s3 <> " steps, converged=" <> show c3 <> ")"

  sub "2d. Branch elimination: if True then x else y → x"
  let e4 = ifE (Fix (BoolF True)) (var "x") (var "y")
  note $ "Input:  " <> show (prettyExpr e4)
  let (r4, _, _) = rewrite defaultConfig defaultRules e4
  pass $ "Output: " <> show (prettyExpr r4)

  sub "2e. Dead-let elimination: let z = 99 in x → x  (z unused)"
  let e5 = letE "z" (lit 99) (var "x")
  note $ "Input:  " <> show (prettyExpr e5)
  let (r5, _, _) = rewrite defaultConfig defaultRules e5
  pass $ "Output: " <> show (prettyExpr r5)

  sub "2f. Eta reduction: (\\f -> (\\x -> f x)) → f  (when x not free in f)"
  let e6 = lam "x" (app (var "f") (var "x"))
  note $ "Input:  " <> show (prettyExpr e6)
  let (r6, _, _) = rewrite defaultConfig defaultRules e6
  pass $ "Output: " <> show (prettyExpr r6)

  sub "2g. generateMutations on bootstrapExpr (hypothesis candidates)"
  let muts = generateMutations defaultRules bootstrapExpr
  note $ "Rules tried: " <> show (length defaultRules)
  note $ "Mutations that fired: " <> show (length muts)
  mapM_ (\(e', m) -> note $ "  [" <> show (mutationType m) <> "] "
                           <> show (mutationTarget m)
                           <> " → " <> show (prettyExpr e')) muts

-- ─────────────────────────────────────────────────────────────────────────────
-- 3. Verification
-- ─────────────────────────────────────────────────────────────────────────────

demoVerification :: IO ()
demoVerification = do
  section "3. Gödel Gate — Formal Verification (Equivalence + Refinements)"

  sub "3a. Equivalent expressions → Verified"
  -- (2 + 3) is equivalent to 5
  let orig1 = binop "+" (lit 2) (lit 3)
  let mut1  = lit 5
  note $ "Original: " <> show (prettyExpr orig1)
  note $ "Mutated:  " <> show (prettyExpr mut1)
  r1 <- verifyEquivalence EquivSpec
          { esName = "2+3 vs 5", esOriginal = orig1, esMutated = mut1
          , esInputDomain = [Map.empty], esSymbolicBound = 1 }
  case r1 of
    Verified msg  -> pass $ "Result: Verified — " <> show msg
    Falsifiable c -> fail' $ "Unexpected falsifiable: " <> show (ceExpected c)
    VTimeout _    -> fail' "Timeout"

  sub "3b. Non-equivalent expressions → Falsifiable with counter-example"
  let orig2 = binop "+" (var "x") (lit 1)
  let mut2  = var "x"
  note $ "Original: " <> show (prettyExpr orig2) <> "  (x + 1)"
  note $ "Mutated:  " <> show (prettyExpr mut2)  <> "  (x)"
  r2 <- verifyEquivalence EquivSpec
          { esName = "x+1 vs x", esOriginal = orig2, esMutated = mut2
          , esInputDomain = [Map.fromList [("x", VInt 5)]]
          , esSymbolicBound = 1 }
  case r2 of
    Falsifiable ce -> pass $ "Result: Falsifiable — counter-example at x="
                           <> show (ceInputs ce)
                           <> " expected=" <> show (ceExpected ce)
                           <> " actual="   <> show (ceActual ce)
    Verified _     -> fail' "Should have been falsifiable"
    VTimeout _     -> fail' "Timeout"

  sub "3c. Semantically preserving simplification of bootstrapExpr → Verified"
  -- bootstrapExpr itself is its own equivalent (identity mutation)
  note $ "Baseline: bootstrapExpr (factorial 10)"
  note $ "Mutated:  same expression (identity — zero node reduction)"
  r3 <- runVerification defaultVerifConfig bootstrapExpr bootstrapExpr
  case r3 of
    Verified msg  -> pass $ "Result: Verified — " <> show msg
    Falsifiable c -> fail' $ "Unexpected falsifiable: " <> show (ceExpected c)
    VTimeout _    -> fail' "Timeout"

  sub "3d. Refinement check — non-negative index predicate"
  note "nonNegIndex predicate: VInt n → n >= 0"
  let good = VInt 42
  let bad  = VInt (-1)
  case checkRefinements [nonNegIndex] good of
    Right () -> pass $ "VInt 42 → passes non-negative check"
    Left vs  -> fail' $ "Unexpected violation: " <> show vs
  case checkRefinements [nonNegIndex] bad of
    Left vs  -> pass $ "VInt (-1) → violates: " <> show vs
    Right () -> fail' "Should have violated"

-- ─────────────────────────────────────────────────────────────────────────────
-- 4. Absolute Zero Self-Play
-- ─────────────────────────────────────────────────────────────────────────────

demoAbsoluteZero :: IO ()
demoAbsoluteZero = do
  section "4. Absolute Zero — Endogenous Task Proposal and Solving"

  sub "4a. Learnability reward curve (Gaussian around capability)"
  note "learnabilityScore(capability=0.3, difficulty=d):"
  let cap = 0.3
  let diffs = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1.0]
  mapM_ (\d -> note $ "  d=" <> show d <> " → score=" <>
               showRound2 (learnabilityScore cap d)) diffs

  sub "4b. Task proposal — Proposer selects task at frontier"
  let prop0 = defaultProposer  -- capability=0.2, exploration=0.15
  note $ "Initial capability estimate: " <> show (propCurrentDifficulty prop0)
  note $ "Exploration rate: " <> show (propExplorationRate prop0)
  note $ "Target difficulty: " <> show (propCurrentDifficulty prop0 + propExplorationRate prop0)
  st0 <- newAgentState (exprToASTNode "demo" bootstrapExpr)
  (task0, _) <- proposeTask st0 prop0
  result $ "Proposed task: " <> show (taskId task0)
           <> " (difficulty=" <> show (taskDifficulty task0) <> ")"

  sub "4c. Solver — attempt the proposed task"
  sr0 <- solveTask task0
  note $ "Task: " <> show (srTaskId sr0)
  note $ "Steps used: " <> show (srStepsUsed sr0)
  case srSolution sr0 of
    Just sol -> pass $ "Solved! Result expression: " <> show (prettyExpr sol)
                     <> "  accuracy=" <> show (srAccuracy sr0)
    Nothing  -> fail' $ "Not solved (accuracy=" <> show (srAccuracy sr0) <> ")"

  sub "4d. Self-play loop — capability adapts over 5 steps"
  note "Running 5 self-play steps; difficulty estimate should rise as tasks are solved:"
  let cfg = defaultSelfPlayConfig st0
  runSelfPlay cfg 5 (propCurrentDifficulty (spProposer cfg))

  sub "4e. Accuracy reward function"
  -- Test with a trivially solvable task (3 + 4)
  let arithTask = Task
        { taskId          = "add-3-4"
        , taskType        = ArithmeticTask
        , taskDifficulty  = 0.05
        , taskDescription = "Compute 3 + 4"
        , taskSpec        = ExprTask (binop "+" (lit 3) (lit 4))
        , taskVerify      = \e env ->
            case evalExpr env e of
              Right (VInt 7) -> True
              _              -> False
        }
  sr1 <- solveTask arithTask
  pass $ "add-3-4: accuracy=" <> show (accuracyReward sr1)
        <> ", steps=" <> show (srStepsUsed sr1)

runSelfPlay :: SelfPlayConfig -> Int -> Double -> IO ()
runSelfPlay _ 0 _ = pure ()
runSelfPlay cfg n cap = do
  (newProp, sr) <- runSelfPlayStep cfg
  note $ "  step " <> show (6-n) <> ": task=" <> show (srTaskId sr)
       <> "  accuracy=" <> show (srAccuracy sr)
       <> "  new_cap=" <> showRound2 (propCurrentDifficulty newProp)
  let cfg' = cfg { spProposer = newProp }
  runSelfPlay cfg' (n-1) (propCurrentDifficulty newProp)

-- ─────────────────────────────────────────────────────────────────────────────
-- 5. Safety Kernel
-- ─────────────────────────────────────────────────────────────────────────────

demoSafetyKernel :: IO ()
demoSafetyKernel = do
  section "5. Safety Kernel — GADT Airlock, STM Rollback, Quorum Proofs"

  st    <- newAgentState (exprToASTNode "root" bootstrapExpr)
  audit <- newAuditLog

  sub "5a. Safe command — no transaction plan, no rollback"
  r1 <- dispatchSafe st audit (Evaluate "1 + 1")
  case r1 of
    Right msg -> pass $ "dispatchSafe (Evaluate): " <> show msg
    Left  err -> fail' $ show err

  r2 <- dispatchSafe st audit (Inspect (exprToASTNode "n1" (lit 42)))
  case r2 of
    Right msg -> pass $ "dispatchSafe (Inspect):  " <> show msg
    Left  err -> fail' $ show err

  sub "5b. Critical command — STM snapshot + commit"
  note "Capturing state before mutation..."
  plan <- mkTransactionPlan st
  note $ "  Snapshot generation: " <> show (tpGeneration plan)
  let mut = Mutation
        { mutationType    = Optimize
        , mutationTarget  = "root"
        , mutationPayload = ExprMutation
            (exprToASTNode "old" bootstrapExpr)
            (exprToASTNode "new" (lit 42))
        }
  r3 <- dispatchCritical st audit (MutateAST mut)
  case r3 of
    Right msg -> pass $ "dispatchCritical (MutateAST): " <> show msg
    Left  err -> fail' $ show err
  ast1 <- atomically (readTVar (stateCurrentAST st))
  note $ "  AST after mutation: id=" <> show (astId ast1)
                                     <> " value=" <> show (astValue ast1)

  sub "5c. STM rollback — O(1) undo via persistent data"
  note "Rolling back to pre-mutation snapshot..."
  rollback st plan
  ast2 <- atomically (readTVar (stateCurrentAST st))
  note $ "  AST after rollback: id=" <> show (astId ast2)
  pass $ "Rollback successful — state restored to generation "
       <> show (tpGeneration plan)

  sub "5d. Existential command — requires QuorumProof (type enforced)"
  note "Attempting Terminate with a VALID quorum proof:"
  proofTs <- (floor <$> getPOSIXTime) :: IO Int64
  let validProof = mockQuorumProof "terminate" proofTs
  r4 <- dispatchExistential st audit (Terminate validProof)
  case r4 of
    Right msg -> pass $ "Terminate with valid proof: " <> show msg
    Left  err -> fail' $ show err

  note "Attempting Terminate with an INVALID quorum proof:"
  let badProof = QuorumProof { quorumHash = "wrong-hash"
                             , quorumTimestamp = proofTs
                             , quorumOperation = "terminate" }
  r5 <- dispatchExistential st audit (Terminate badProof)
  case r5 of
    Left err  -> pass $ "Correctly rejected: " <> show err
    Right msg -> fail' $ "Should have been rejected: " <> show msg

  sub "5e. Audit log — append-only intent/impact record"
  entries <- readIORef audit
  note $ "Audit log entries recorded: " <> show (length entries)
  mapM_ (\e -> note $ "  [" <> show (aeLevel e) <> "] " <> show (aeMessage e))
        (reverse (take 6 entries))

  sub "5f. Type safety demonstration — GADT prevents wrong-level dispatch"
  note "The following is REJECTED at compile time (not runtime):"
  note "  dispatchSafe   st audit (MutateAST mut)   -- type error: Command 'Critical ≠ Command 'Safe"
  note "  dispatchCritical st audit (Terminate proof) -- type error: Command 'Existential ≠ Command 'Critical"
  pass "GHC enforces safety levels statically — no runtime guard needed."

-- ─────────────────────────────────────────────────────────────────────────────
-- 6. Full Evolution Loop
-- ─────────────────────────────────────────────────────────────────────────────

demoEvolutionLoop :: IO ()
demoEvolutionLoop = do
  section "6. Full Evolution Loop — 8 Zero Data Cycles"

  st    <- newAgentState (exprToASTNode "root" bootstrapExpr)
  audit <- newAuditLog
  hdl   <- openArchive InMemory
  cfg <- defaultCycleConfig st audit hdl

  note "Running 8 cycles (each: propose→hypothesise→verify→classify→test→persist)..."
  note ""

  -- Run with brief per-cycle output
  results <- mapM (\i -> runCycleVerbose cfg i) [1..8 :: Int]

  let passed = length [() | (_, True) <- results]
  let failed = length [() | (_, False) <- results]

  putStrLn ""
  putStrLn $ "  " <> replicate 55 '─'
  note $ "Cycles completed: 8"
  note $ "Full pipeline (all 6 steps): " <> show passed
  note $ "Short-circuited at Verify:   " <> show failed

  sub "Archive statistics after 8 cycles"
  entries <- atomically (getAll (stateArchive st))
  let stats = computeStats entries
  note $ "Total archived:     " <> show (asTotal stats)
  note $ "Passed (committed): " <> show (asPassed stats)
  note $ "Failed (stepping):  " <> show (asFailed stats)
  note $ "Best score:         " <> showRound2 (asBestScore stats)
  note $ "Mean score:         " <> showRound2 (asMeanScore stats)

  sub "Top 3 archive entries by score"
  let top3 = take 3 (sortByScore entries)
  mapM_ (\e -> note $ "  [" <> (if entryPassed e then "✓" else "✗") <> "]"
                    <> " score=" <> showRound2 (entryScore e)
                    <> " gen="   <> show (entryGeneration e)
                    <> " code="  <> take 40 (show (entryCode e)))
        top3

runCycleVerbose :: CycleConfig -> Int -> IO (Int, Bool)
runCycleVerbose cfg i = do
  steps <- runCycle cfg ExprPhase
  let allOk  = all stepSuccess steps
  let nSteps = length steps
  putStrLn $ "  cycle " <> pad 2 i <> ": "
           <> (if allOk then "✓ all " <> show nSteps <> " steps passed"
               else "✗ short-circuited after step " <> show nSteps
                    <> " (" <> show (stepName (last steps)) <> ")")
  pure (i, allOk)

sortByScore :: [ArchiveEntry] -> [ArchiveEntry]
sortByScore [] = []
sortByScore (x:xs) =
  sortByScore [e | e <- xs, entryScore e >= entryScore x]
  <> [x]
  <> sortByScore [e | e <- xs, entryScore e < entryScore x]

-- ─────────────────────────────────────────────────────────────────────────────
-- Utilities
-- ─────────────────────────────────────────────────────────────────────────────

showRound2 :: Double -> String
showRound2 x = show (fromIntegral (round (x * 100) :: Int) / 100.0 :: Double)

pad :: Int -> Int -> String
pad w n = let s = show n in replicate (w - length s) ' ' <> s

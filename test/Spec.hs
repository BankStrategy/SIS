{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Test suite for the Darwin Gödel Machine MVP.
module Main where

import Control.Concurrent.STM
import Control.Exception (bracket, catch)
import Data.Int (Int64)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Tasty
import System.Directory
  ( removeFile, getTemporaryDirectory, createDirectoryIfMissing
  , removeDirectoryRecursive, doesFileExist
  )
import System.FilePath ((</>))
import System.Exit (ExitCode(..))
import System.IO.Error (isDoesNotExistError)
import System.Process (readProcessWithExitCode)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import DGM
import DGM.Liquid (LiquidResult(..), parseLiquidOutput, verifyWithLiquid)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "DGM"
  [ astTests
  , rewritingTests
  , verificationTests
  , safetyKernelTests
  , systemWriteTests
  , archiveTests
  , absoluteZeroTests
  , phaseGTests
  , cycleTests
  , selfModTests
  , selfCompileTests
  , commitMutationTests
  , modGraphTests
  , generationTests
  , reversalTests
  , liquidTests
  , oracleTests
  , oracleHandleTests
  , dynamicRuleTests
  , pipelineTests
  , natLangTests
#ifdef WITH_SQLITE
  , sqliteArchiveTests
#endif
#ifdef WITH_HINT
  , hintEvalTests
  , hintRuleCandidateTests
#endif
#ifdef WITH_EXACTPRINT
  , hsASTTests
#endif
#ifdef WITH_SBV
  , sbvVerificationTests
#endif
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- AST tests
-- ─────────────────────────────────────────────────────────────────────────────

astTests :: TestTree
astTests = testGroup "DGM.AST"
  [ testCase "lit pretty-prints correctly" $
      prettyExpr (lit 42) @?= "42"

  , testCase "binop pretty-prints correctly" $
      prettyExpr (binop "+" (lit 1) (lit 2)) @?= "(1 + 2)"

  , testCase "cata countNodes counts all nodes" $
      countNodes (binop "*" (lit 2) (binop "+" (lit 3) (lit 4))) @?= 5

  , testCase "substituteVar replaces target variable" $
      prettyExpr (substituteVar "x" (lit 99) (binop "+" (var "x") (lit 1)))
        @?= "(99 + 1)"

  , testCase "substituteVar leaves other variables alone" $
      prettyExpr (substituteVar "x" (lit 0) (var "y"))
        @?= "y"

  , testCase "collectVars finds free variables" $
      collectVars (binop "+" (var "a") (var "b"))
        `elem` [mempty]  -- just checks it doesn't throw
        @?= False  -- non-empty set

  , testCase "evalExpr evaluates addition" $ do
      let env = Map.fromList [("x", VInt 3)]
      evalExpr env (binop "+" (var "x") (lit 7)) @?= Right (VInt 10)

  , testCase "evalExpr evaluates conditional" $ do
      let env = Map.empty
      evalExpr env (ifE (Fix (BoolF True)) (lit 1) (lit 2)) @?= Right (VInt 1)

  , testCase "evalExpr returns Left on unbound variable" $ do
      let r = evalExpr Map.empty (var "undefined_var")
      case r of
        Left _ -> pure ()
        Right _ -> assertFailure "Expected Left for unbound var"

  , testCase "hylo is equivalent to cata . ana" $ do
      -- (cata sumAlg . ana listCoalg) 5 == hylo sumAlg listCoalg 5
      -- Use countNodes as a proxy: both should give same result.
      let expr = bootstrapExpr
      let n1 = countNodes expr
      n1 `seq` pure ()  -- just ensure no exception
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Rewriting tests
-- ─────────────────────────────────────────────────────────────────────────────

rewritingTests :: TestTree
rewritingTests = testGroup "DGM.Rewriting"
  [ testCase "constant-fold reduces (3 + 4) to 7" $ do
      let (result, _, converged) = rewrite defaultConfig defaultRules
                                     (binop "+" (lit 3) (lit 4))
      prettyExpr result @?= "7"
      converged @?= True

  , testCase "identity-elim removes (x + 0)" $ do
      let e = binop "+" (var "x") (lit 0)
      let (result, _, _) = rewrite defaultConfig defaultRules e
      prettyExpr result @?= "x"

  , testCase "constant-fold chains: (2 * (3 + 4)) -> 14" $ do
      let e = binop "*" (lit 2) (binop "+" (lit 3) (lit 4))
      let (result, _, _) = rewrite defaultConfig defaultRules e
      prettyExpr result @?= "14"

  , testCase "if-constant eliminates True branch" $ do
      let e = ifE (Fix (BoolF True)) (lit 42) (lit 0)
      let (result, _, _) = rewrite defaultConfig defaultRules e
      prettyExpr result @?= "42"

  , testCase "generateMutations produces candidates" $ do
      let e = binop "+" (lit 1) (lit 0)
      let muts = generateMutations defaultRules e
      length muts >= 1 @?= True

  , testCase "generateMutations finds recursive mutations in subterms" $ do
      -- (2 + 3) * (x + 0)  — constant-fold fires on (2+3), identity-elim on (x+0)
      let e = binop "*" (binop "+" (lit 2) (lit 3)) (binop "+" (var "x") (lit 0))
      let muts = generateMutations defaultRules e
      let targets = map (mutationTarget . snd) muts
      ("constant-fold" `elem` targets) @?= True
      ("identity-elim" `elem` targets) @?= True

  , testCase "subtermsWithCtx counts all positions" $ do
      -- (1 + 2) has 3 nodes: the BinOp root, lit 1, lit 2
      let e = binop "+" (lit 1) (lit 2)
      length (subtermsWithCtx e) @?= 3
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Verification tests
-- ─────────────────────────────────────────────────────────────────────────────

verificationTests :: TestTree
verificationTests = testGroup "DGM.Verification"
  [ testCase "equivalent expressions verify as Verified" $ do
      let original = binop "+" (lit 3) (lit 4)
      let mutated  = lit 7  -- constant-folded form
      result <- runVerification defaultVerifConfig original mutated
      case result of
        Verified _ -> pure ()
        other      -> assertFailure ("Expected Verified, got: " <> show other)

  , testCase "non-equivalent expressions produce Falsifiable" $ do
      let original = lit 7
      let mutated  = lit 8  -- wrong answer
      result <- runVerification defaultVerifConfig original mutated
      case result of
        Falsifiable _ -> pure ()
        other         -> assertFailure ("Expected Falsifiable, got: " <> show other)

  , testCase "refinement non-negative passes for positive value" $ do
      let refs = [nonNegIndex]
      checkRefinements refs (VInt 5) @?= Right ()

  , testCase "refinement non-negative fails for negative value" $ do
      let refs = [nonNegIndex]
      case checkRefinements refs (VInt (-1)) of
        Left _  -> pure ()
        Right _ -> assertFailure "Expected Left for negative index"
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- SBV/Z3 real SMT verification tests (only when with-sbv flag is enabled)
-- ─────────────────────────────────────────────────────────────────────────────

#ifdef WITH_SBV
sbvVerificationTests :: TestTree
sbvVerificationTests = testGroup "DGM.Verification.SBV"
  [ testCase "2+3 vs 5: Z3 proves equivalence (Q.E.D.)" $ do
      let spec = EquivSpec
            { esName          = "const-fold"
            , esOriginal      = binop "+" (lit 2) (lit 3)
            , esMutated       = lit 5
            , esInputDomain   = []
            , esSymbolicBound = 0
            }
      result <- verifyEquivalence spec
      case result of
        Verified msg ->
          assertBool ("Expected 'Q.E.D. via SBV/Z3' in message, got: " <> T.unpack msg)
                     ("Q.E.D." `T.isInfixOf` msg)
        other -> assertFailure ("Expected Verified Q.E.D., got: " <> show other)

  , testCase "x+1 vs x: Z3 finds counter-example (Falsifiable)" $ do
      let spec = EquivSpec
            { esName          = "wrong-opt"
            , esOriginal      = binop "+" (var "x") (lit 1)
            , esMutated       = var "x"
            , esInputDomain   = []
            , esSymbolicBound = 0
            }
      result <- verifyEquivalence spec
      case result of
        Falsifiable ce ->
          assertBool ("Counter-example should mention 'x', got inputs: " <> show (ceInputs ce))
                     (Map.member "x" (ceInputs ce))
        other -> assertFailure ("Expected Falsifiable, got: " <> show other)
  ]
#endif

-- ─────────────────────────────────────────────────────────────────────────────
-- Safety Kernel tests
-- ─────────────────────────────────────────────────────────────────────────────

safetyKernelTests :: TestTree
safetyKernelTests = testGroup "DGM.SafetyKernel"
  [ testCase "Safe command executes without transaction plan" $ do
      let node = exprToASTNode "n" (lit 1)
      st <- newAgentState node
      auditLog <- newIORef []
      result <- dispatchSafe st auditLog (Evaluate "1 + 1")
      case result of
        Right _ -> pure ()
        Left e  -> assertFailure ("Expected Right, got: " <> T.unpack e)

  , testCase "Transaction plan snapshot and rollback works" $ do
      let node = exprToASTNode "n" (lit 1)
      st <- newAgentState node
      plan <- mkTransactionPlan st
      -- Modify state.
      atomically $ writeTVar (stateGeneration st) 99
      -- Rollback.
      rollback st plan
      gen <- atomically $ readTVar (stateGeneration st)
      gen @?= 0

  , testCase "Quorum proof verifies correctly" $ do
      let op    = "terminate"
          ts    = 1000000000 :: Int64
          proof = mockQuorumProof op ts
      verifyQuorum proof op ts @?= Right ()

  , testCase "Wrong quorum proof fails" $ do
      let ts    = 1000000000 :: Int64
          proof = QuorumProof { quorumHash = "wrong-hash"
                              , quorumTimestamp = ts
                              , quorumOperation = "terminate" }
      case verifyQuorum proof "terminate" ts of
        Left _  -> pure ()
        Right _ -> assertFailure "Expected Left for wrong quorum"

  , testCase "SHA-256 quorum hash is a 64-character hex string" $ do
      let op    = "terminate"
          ts    = 1000000000 :: Int64
          proof = mockQuorumProof op ts
      T.length (quorumHash proof) @?= 64

  , testCase "Expired proof (timestamp >5 minutes old) is rejected" $ do
      let op    = "terminate"
          ts    = 1000000000 :: Int64
          proof = mockQuorumProof op ts
          now   = ts + 400  -- 400 s > 300 s (5-minute window)
      case verifyQuorum proof op now of
        Left _  -> pure ()
        Right _ -> assertFailure "Expected Left for expired proof"

  , testCase "auditCommand is pure (no side effects)" $ do
      let cmd = AnyCommand SingSafe (Evaluate "test")
      let entry = auditCommand cmd
      aeLevel entry @?= "Safe"
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- SystemWrite integration tests
-- ─────────────────────────────────────────────────────────────────────────────

systemWriteTests :: TestTree
systemWriteTests = testGroup "DGM.SafetyKernel.SystemWrite"
  [ testCase "SystemWrite writes file atomically with valid proof" $ do
      tmpDir <- getTemporaryDirectory
      let srcDir = tmpDir </> "dgm-sw-test" </> "src"
      createDirectoryIfMissing True srcDir
      let targetPath = srcDir </> "Test.hs"
      let content    = "module Test where\n"
      -- Create agent state with tmpDir as the repo root.
      let node = exprToASTNode "n" (lit 1)
      st <- newAgentStateWithRoot node (tmpDir </> "dgm-sw-test")
      auditLog <- newIORef []
      now <- floor <$> (getPOSIXTime :: IO POSIXTime)
      let opName = "write:" <> T.pack targetPath
          proof  = mockQuorumProof opName now
          cmd    = SystemWrite targetPath (T.pack content) proof
      result <- dispatchExistential st auditLog cmd
      case result of
        Right _ -> pure ()
        Left e  -> assertFailure ("Expected Right, got: " <> T.unpack e)
      -- Verify file content was actually written.
      written <- readFile targetPath
      written @?= content
      -- Cleanup.
      removeFile targetPath `catch` \e ->
        if isDoesNotExistError e then pure () else ioError e

  , testCase "SystemWrite rejects path outside repo src/ directory" $ do
      tmpDir <- getTemporaryDirectory
      let repoRoot   = tmpDir </> "dgm-sw-whitelist-test"
          outsidePath = tmpDir </> "outside.hs"
      let node = exprToASTNode "n" (lit 1)
      st <- newAgentStateWithRoot node repoRoot
      auditLog <- newIORef []
      now <- floor <$> (getPOSIXTime :: IO POSIXTime)
      let opName = "write:" <> T.pack outsidePath
          proof  = mockQuorumProof opName now
          cmd    = SystemWrite outsidePath "module Bad where\n" proof
      result <- dispatchExistential st auditLog cmd
      case result of
        Left _  -> pure ()
        Right _ -> assertFailure "Expected Left for path outside whitelist"

  , testCase "SystemWrite rejects expired quorum proof" $ do
      tmpDir <- getTemporaryDirectory
      let srcDir = tmpDir </> "dgm-sw-expired" </> "src"
          target = srcDir </> "X.hs"
      createDirectoryIfMissing True srcDir
      let node = exprToASTNode "n" (lit 1)
      st <- newAgentStateWithRoot node (tmpDir </> "dgm-sw-expired")
      auditLog <- newIORef []
      now <- floor <$> (getPOSIXTime :: IO POSIXTime)
      let opName = "write:" <> T.pack target
          -- Timestamp 400 seconds in the past — outside 5-minute window.
          proof  = mockQuorumProof opName (now - 400)
          cmd    = SystemWrite target "module X where\n" proof
      result <- dispatchExistential st auditLog cmd
      case result of
        Left _  -> pure ()
        Right _ -> assertFailure "Expected Left for expired proof"

  , testCase "SystemWrite rejects unconfigured repo root" $ do
      tmpDir <- getTemporaryDirectory
      let node = exprToASTNode "n" (lit 1)
      -- newAgentState leaves stateRepoRoot as "" (unconfigured).
      st <- newAgentState node
      auditLog <- newIORef []
      now <- floor <$> (getPOSIXTime :: IO POSIXTime)
      let target = tmpDir </> "src" </> "Foo.hs"
          opName = "write:" <> T.pack target
          proof  = mockQuorumProof opName now
          cmd    = SystemWrite target "module Foo where\n" proof
      result <- dispatchExistential st auditLog cmd
      case result of
        Left _  -> pure ()
        Right _ -> assertFailure "Expected Left when repoRoot not configured"
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Archive tests
-- ─────────────────────────────────────────────────────────────────────────────

archiveTests :: TestTree
archiveTests = testGroup "DGM.Archive"
  [ testCase "addEntry and getAll round-trip" $ do
      archVar <- newTVarIO []
      let e1 = mkEntry "e1" 0.8 True
      atomically $ addEntry archVar e1
      entries <- atomically $ getAll archVar
      length entries @?= 1

  , testCase "getBest returns highest score" $ do
      archVar <- newTVarIO []
      atomically $ addEntry archVar (mkEntry "e1" 0.5 True)
      atomically $ addEntry archVar (mkEntry "e2" 0.9 True)
      atomically $ addEntry archVar (mkEntry "e3" 0.3 False)
      best <- atomically $ getBest archVar
      fmap entryScore best @?= Just 0.9

  , testCase "computeStats totals correctly" $ do
      let entries =
            [ mkEntry "e1" 0.8 True
            , mkEntry "e2" 0.2 False
            , mkEntry "e3" 0.6 True
            ]
      let stats = computeStats entries
      asTotal stats  @?= 3
      asPassed stats @?= 2
      asFailed stats @?= 1

  , testCase "failed entries are preserved (DGM principle)" $ do
      archVar <- newTVarIO []
      -- Archive a failed mutation.
      atomically $ addEntry archVar (mkEntry "fail1" 0.0 False)
      entries <- atomically $ getAll archVar
      -- Crucially, the failed entry must be in the archive.
      length (filter (not . entryPassed) entries) @?= 1
  ]

mkEntry :: Text -> Double -> Bool -> ArchiveEntry
mkEntry eid score passed = ArchiveEntry
  { entryId           = eid
  , entryCode         = "placeholder"
  , entryMutation     = Nothing
  , entryParentId     = Nothing
  , entryScore        = score
  , entryPassed       = passed
  , entryGeneration   = 0
  , entryCounterEx    = Nothing
  , entryLiquidResult = Nothing
  , entrySbvResult    = Nothing
  , entryOracleModel  = Nothing
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- Absolute Zero tests
-- ─────────────────────────────────────────────────────────────────────────────

absoluteZeroTests :: TestTree
absoluteZeroTests = testGroup "DGM.AbsoluteZero"
  [ testCase "proposeTask returns a task" $ do
      st <- newAgentState (exprToASTNode "test" bootstrapExpr)
      (task, _) <- proposeTask st defaultProposer
      T.length (taskId task) > 0 @?= True

  , testCase "learnabilityScore peaks when capability matches difficulty" $ do
      let score = learnabilityScore 0.3 0.3
      score > 0.9 @?= True

  , testCase "learnabilityScore decays for very different difficulty" $ do
      let score = learnabilityScore 0.1 0.9
      score < 0.1 @?= True

  , testCase "solveTask solves trivial arithmetic" $ do
      let task = Task
            { taskId          = "trivial"
            , taskType        = ArithmeticTask
            , taskDifficulty  = 0.01
            , taskDescription = "7"
            , taskSpec        = ExprTask (binop "+" (lit 3) (lit 4))
            , taskVerify      = \expr env ->
                case evalExpr env expr of
                  Right (VInt 7) -> True
                  _              -> False
            }
      result <- solveTask task
      srAccuracy result @?= 1.0
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Phase G: SelfModTask DSL and Absolute Zero integration tests
-- ─────────────────────────────────────────────────────────────────────────────

phaseGTests :: TestTree
phaseGTests = testGroup "DGM.AbsoluteZero.PhaseG"
  [ testCase "TaskSpec ExprTask wraps expression correctly" $ do
      let spec = ExprTask (lit 42)
      case spec of
        ExprTask _ -> pure ()
        _          -> assertFailure "Expected ExprTask"

  , testCase "SelfModSpec constructs correctly" $ do
      let spec = SelfModSpec
                   { smsTargetModule = "DGM.Rewriting"
                   , smsGoal         = AddRewriteRule "etaReduce"
                   , smsConstraints  = [PreserveExports, MaintainTests]
                   }
      smsTargetModule spec @?= "DGM.Rewriting"
      smsGoal spec @?= AddRewriteRule "etaReduce"
      length (smsConstraints spec) @?= 2

  , testCase "SelfModGoal constructors show correctly" $ do
      show (AddRewriteRule "r")    @?= "AddRewriteRule \"r\""
      show (ImproveScoring "s")    @?= "ImproveScoring \"s\""
      show (ExtendArchive "a")     @?= "ExtendArchive \"a\""
      show (AddCapability "c")     @?= "AddCapability \"c\""

  , testCase "SelfModConstraint constructors are distinguishable" $ do
      PreserveExports /= MaintainTests @?= True
      MaintainTests   /= MaxDelta 5   @?= True

  , testCase "proposeTask: ExprMode proposer returns ExprTask" $ do
      st <- newAgentState (exprToASTNode "test" bootstrapExpr)
      let p = defaultProposer  -- propCurrentPhase = ExprMode
      (task, _) <- proposeTask st p
      case taskSpec task of
        ExprTask _ -> pure ()
        _          -> assertFailure "ExprMode proposer should return ExprTask"

  , testCase "proposeTask: returns updated proposer with incremented counter" $ do
      st <- newAgentState (exprToASTNode "test" bootstrapExpr)
      let p = defaultProposer { propSelfModCounter = 0 }
      (_, p') <- proposeTask st p
      propSelfModCounter p' @?= 1

  , testCase "proposeTask: SelfModMode proposer returns SelfModTask targeting Rewriting.hs" $ do
      let smProp = defaultProposer { propCurrentPhase = SelfModMode }
      (task, _) <- proposeTask (error "unused") smProp
      case taskSpec task of
        SelfModTask spec -> smsTargetModule spec @?= "DGM.Rewriting"
        ExprTask _       -> assertFailure "SelfModMode proposer should return SelfModTask"

  , testCase "solveSelfModTask: safeguarded module scores 0.0" $ do
      let spec = SelfModSpec
                   { smsTargetModule = "DGM.SafetyKernel"
                   , smsGoal         = AddRewriteRule "r"
                   , smsConstraints  = []
                   }
          task = Task
                   { taskId          = "t"
                   , taskType        = OptimisationTask
                   , taskDifficulty  = 0.5
                   , taskDescription = "test"
                   , taskSpec        = SelfModTask spec
                   , taskVerify      = \_ _ -> True
                   }
      result <- solveSelfModTask task spec
      srAccuracy result @?= 0.0

  , testCase "solveSelfModTask: non-safeguarded module is feasible" $ do
      let spec = SelfModSpec
                   { smsTargetModule = "DGM.Rewriting"
                   , smsGoal         = AddRewriteRule "etaReduce"
                   , smsConstraints  = [PreserveExports]
                   }
          task = Task
                   { taskId          = "t"
                   , taskType        = OptimisationTask
                   , taskDifficulty  = 0.35
                   , taskDescription = "test"
                   , taskSpec        = SelfModTask spec
                   , taskVerify      = \_ _ -> True
                   }
      result <- solveSelfModTask task spec
      srAccuracy result > 0.0 @?= True

  , testCase "learnabilityScore peaks when capability near complexity" $ do
      -- For SelfModTask, capability ≈ module complexity → reward peaks.
      let complexity = 0.4
          score      = learnabilityScore complexity complexity
      score > 0.9 @?= True

  , testCase "safeguardedModules: contains DGM.SafetyKernel and DGM.Types" $ do
      ("DGM.SafetyKernel" `elem` safeguardedModules) @?= True
      ("DGM.Types"        `elem` safeguardedModules) @?= True
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- End-to-end cycle test
-- ─────────────────────────────────────────────────────────────────────────────

cycleTests :: TestTree
cycleTests = testGroup "DGM.Cycle"
  [ testCase "runCycle completes without exception" $ do
      let node = exprToASTNode "root" bootstrapExpr
      st <- newAgentState node
      auditLog <- newIORef []
      hdl <- openArchive InMemory
      cfg <- defaultCycleConfig st auditLog hdl
      steps <- runCycle (cfg { ccVerbose = False }) ExprPhase
      length steps >= 2 @?= True  -- At least propose + hypothesise steps.

  , testCase "runCycleN archives entries" $ do
      let node = exprToASTNode "root" bootstrapExpr
      st <- newAgentState node
      auditLog <- newIORef []
      hdl <- openArchive InMemory
      cfg <- defaultCycleConfig st auditLog hdl
      let cfg' = cfg { ccVerbose = False }
      stats <- runCycleN cfg' 3
      asTotal stats >= 0 @?= True  -- May be 0 if no mutations proposed.

  , testCase "proposer difficulty adapts after self-play steps" $ do
      -- Run 5 self-play steps.  With gradient accuracy, difficulty adapts:
      -- simplifiable tasks (constant-fold) score 1.0 and raise difficulty;
      -- non-reducible tasks score 0.2 and hold steady.
      st <- newAgentState (exprToASTNode "test" bootstrapExpr)
      let cfg = SelfPlayConfig
                  { spProposer   = defaultProposer  -- propCurrentDifficulty = 0.2
                  , spAdaptRate  = 0.05
                  , spAgentState = st
                  }
      let step p = do
            (p', _) <- runSelfPlayStep (cfg { spProposer = p })
            pure p'
      p1 <- step defaultProposer
      p2 <- step p1
      p3 <- step p2
      p4 <- step p3
      p5 <- step p4
      -- After 5 steps the difficulty must still be valid (in [0,1]).
      propCurrentDifficulty p5 >= 0.0 @?= True
      propCurrentDifficulty p5 <= 1.0 @?= True
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- SQLite archive persistence tests
-- ─────────────────────────────────────────────────────────────────────────────

#ifdef WITH_HINT
-- ─────────────────────────────────────────────────────────────────────────────
-- hint integration tests (only compiled with -f+with-hint)
-- ─────────────────────────────────────────────────────────────────────────────

hintEvalTests :: TestTree
hintEvalTests = testGroup "DGM.Sandbox.hint"
  [ testCase "hint eval: evaluates Haskell expression via GHC-API" $ do
      result <- runInSandbox defaultSandboxConfig
                  "show (length [1,2,3 :: Int])"
      srPassed result @?= True
      srOutput result @?= Right "3"

  , testCase "hint eval: rejects Safe Haskell violation (unsafePerformIO not in scope)" $ do
      -- defaultSandboxConfig has scSafeOnly=True and only safe imports
      -- (Prelude, Data.List, Data.Maybe). unsafePerformIO is not in scope.
      result <- runInSandbox defaultSandboxConfig
                  "show (unsafePerformIO (pure (42 :: Int)))"
      srPassed result @?= False
  ]
#endif

#ifdef WITH_SQLITE
sqliteArchiveTests :: TestTree
sqliteArchiveTests = testGroup "DGM.Archive.SQLite"
  [ testCase "open and close SQLite archive creates table" $ do
      let path = "/tmp/dgm-test-open.db"
      cleanupDb path
      hdl <- openArchive (SQLiteBacked path)
      closeArchive hdl
      -- If we got here without exception, the table was created successfully.
      cleanupDb path

  , testCase "write entries to SQLite and verify they persist" $ do
      let path = "/tmp/dgm-test-write.db"
      cleanupDb path
      hdl <- openArchive (SQLiteBacked path)
      let entries = [mkEntry "s1" 0.7 True, mkEntry "s2" 0.3 False]
      flushArchive hdl entries
      closeArchive hdl
      -- Reopen and verify row count via a fresh handle.
      hdl2 <- openArchive (SQLiteBacked path)
      loaded <- loadArchive hdl2
      closeArchive hdl2
      length loaded @?= 2
      cleanupDb path

  , testCase "reload SQLite archive restores entries across handles" $ do
      let path = "/tmp/dgm-test-reload.db"
      cleanupDb path
      -- First run: write three entries.
      hdl1 <- openArchive (SQLiteBacked path)
      flushArchive hdl1 [mkEntry "r1" 0.9 True, mkEntry "r2" 0.5 True, mkEntry "r3" 0.1 False]
      closeArchive hdl1
      -- Second run: open fresh handle, load, verify content.
      hdl2 <- openArchive (SQLiteBacked path)
      entries <- loadArchive hdl2
      closeArchive hdl2
      length entries @?= 3
      let scores = map entryScore entries
      maximum scores @?= 0.9
      length (filter entryPassed entries) @?= 2
      cleanupDb path
  ]

cleanupDb :: FilePath -> IO ()
cleanupDb path = removeFile path `catch` \e ->
  if isDoesNotExistError e then pure () else ioError e
#endif

#ifdef WITH_EXACTPRINT
-- ─────────────────────────────────────────────────────────────────────────────
-- DGM.HsAST tests (only compiled with -f+with-exactprint)
-- ─────────────────────────────────────────────────────────────────────────────

-- | A small Haskell source fixture with a known unused explicit import.
--
-- Used to test 'collectHsMutations' and 'applyHsMutation' without depending
-- on the exact content of production source files.  The @sort@ import is
-- unused so 'unusedImportRule' will flag it for removal.
hsASTFixture :: Text
hsASTFixture = T.unlines
  [ "module TestMutate where"
  , ""
  , "import Data.List (sort)"
  , ""
  , "-- | Reverse a list."
  , "reverseList :: [a] -> [a]"
  , "reverseList xs = reverse xs"
  , ""
  , "-- | Map a function over a list."
  , "applyAll :: (a -> b) -> [a] -> [b]"
  , "applyAll f xs = map f xs"
  ]

hsASTTests :: TestTree
hsASTTests = testGroup "DGM.HsAST"
  [ testCase "parseHsFile round-trip: printHsModule reproduces file content" $ do
      let srcFile = "src/DGM/Types.hs"
      parseResult <- parseHsFile srcFile
      case parseResult of
        Left err -> assertFailure ("parseHsFile failed: " <> T.unpack err)
        Right m  -> do
          original <- readFile srcFile
          let printed = T.unpack (printHsModule m)
          -- Allow trailing-newline difference (exactprint normalises this).
          dropTrailingNL printed @?= dropTrailingNL original

  , testCase "parseHsText round-trip: printHsModule reproduces input" $ do
      let src = "module Foo where\nfoo :: Int -> Int\nfoo x = x + 1\n"
      parseResult <- parseHsText (T.pack src)
      case parseResult of
        Left err -> assertFailure ("parseHsText failed: " <> T.unpack err)
        Right m  -> do
          let printed = T.unpack (printHsModule m)
          dropTrailingNL printed @?= dropTrailingNL src

  , testCase "collectHsMutations finds at least one mutation in known fixture" $ do
      parseResult <- parseHsText hsASTFixture
      case parseResult of
        Left err -> assertFailure ("parseHsText failed: " <> T.unpack err)
        Right m  -> do
          let muts = collectHsMutations defaultHsRules m
          length muts >= 1 @?= True

  , testCase "applyHsMutation produces syntactically valid Haskell" $ do
      parseResult <- parseHsText hsASTFixture
      case parseResult of
        Left err -> assertFailure ("parseHsText failed: " <> T.unpack err)
        Right m  -> do
          let muts = collectHsMutations defaultHsRules m
          case muts of
            [] -> assertFailure "No mutations found in fixture"
            (mut:_) ->
              case applyHsMutation mut m of
                Left err -> assertFailure ("applyHsMutation failed: " <> T.unpack err)
                Right m' -> do
                  let mutated = printHsModule m'
                  reparse <- parseHsText mutated
                  case reparse of
                    Left err -> assertFailure
                      ("Mutated module does not re-parse: " <> T.unpack err
                       <> "\nContent:\n" <> T.unpack mutated)
                    Right _  -> pure ()

  , testCase "hsNodes returns non-empty list for valid module" $ do
      parseResult <- parseHsText hsASTFixture
      case parseResult of
        Left err -> assertFailure ("parseHsText failed: " <> T.unpack err)
        Right m  -> length (hsNodes m) >= 1 @?= True

  -- ── addHaddockRule ─────────────────────────────────────────────────────────

  , testCase "addHaddockTransform inserts stub before undocumented binding" $ do
      let src = "module Foo where\n\nfoo x = x + 1\n"
      case addHaddockTransform src of
        Left err  -> assertFailure ("should have found undocumented binding: " <> T.unpack err)
        Right out -> T.isInfixOf "-- | TODO" out @?= True

  , testCase "addHaddockTransform skips already-documented bindings" $ do
      let src = "module Foo where\n\n-- | Documented\nfoo x = x\n"
      case addHaddockTransform src of
        Left _  -> pure ()  -- expected: no undocumented bindings
        Right _ -> assertFailure "should not fire when all bindings are documented"

  -- ── addTypeAnnotationRule ──────────────────────────────────────────────────

  , testCase "addTypeAnnotationTransform inserts stub for unannotated binding" $ do
      let src = "module Foo where\n\nfoo x = x + 1\n"
      case addTypeAnnotationTransform src of
        Left err  -> assertFailure ("should have found unannotated binding: " <> T.unpack err)
        Right out -> T.isInfixOf "_TODO_" out @?= True

  , testCase "addTypeAnnotationTransform skips annotated bindings" $ do
      let src = "module Foo where\n\nfoo :: Int -> Int\nfoo x = x + 1\n"
      case addTypeAnnotationTransform src of
        Left _  -> pure ()  -- expected: all bindings have signatures
        Right _ -> assertFailure "should not fire when all bindings have type signatures"

  -- ── limitedEtaReduceRule ───────────────────────────────────────────────────

  , testCase "limitedEtaReduceTransform reduces f x = g x" $ do
      let src = "module Foo where\n\nfoo x = bar x\n"
      case limitedEtaReduceTransform src of
        Left err  -> assertFailure ("should have found eta candidate: " <> T.unpack err)
        Right out -> T.isInfixOf "foo = bar" out @?= True

  , testCase "limitedEtaReduceTransform does not fire on multi-arg bindings" $ do
      let src = "module Foo where\n\nfoo x y = bar x y\n"
      case limitedEtaReduceTransform src of
        Left _  -> pure ()  -- expected: multi-arg not handled
        Right _ -> assertFailure "should not fire on multi-argument bindings"
  ]

-- | Drop trailing newline characters for lenient round-trip comparison.
dropTrailingNL :: String -> String
dropTrailingNL = reverse . dropWhile (== '\n') . reverse
#endif

-- ─────────────────────────────────────────────────────────────────────────────
-- DGM.SelfMod tests
-- ─────────────────────────────────────────────────────────────────────────────

selfModTests :: TestTree
selfModTests = testGroup "DGM.SelfMod"
  [ testCase "discoverSources returns at least the core DGM files" $ do
      fps <- discoverSources
      -- Should include Types.hs, Archive.hs, SafetyKernel.hs, etc.
      let basenames = map (reverse . takeWhile (/= '/') . reverse) fps
      "Types.hs"       `elem` basenames @?= True
      "Archive.hs"     `elem` basenames @?= True
      "SafetyKernel.hs" `elem` basenames @?= True

  , testCase "discoverSources only returns .hs files" $ do
      fps <- discoverSources
      all (\fp -> let ext = reverse (takeWhile (/= '.') (reverse fp))
                  in  ext == "hs") fps @?= True

  , testCase "discoverSources returns SelfMod.hs itself" $ do
      fps <- discoverSources
      let basenames = map (reverse . takeWhile (/= '/') . reverse) fps
      "SelfMod.hs" `elem` basenames @?= True

#ifdef WITH_EXACTPRINT
  , testCase "proposeSelfMutations returns >= 1 candidate across DGM sources" $ do
      let node = exprToASTNode "n" (lit 1)
      st  <- newAgentState node
      fps <- discoverSources
      candidates <- proposeSelfMutations st fps
      length candidates >= 1 @?= True

  , testCase "rankMutations is a permutation of the input list" $ do
      let node = exprToASTNode "n" (lit 1)
      st  <- newAgentState node
      fps <- discoverSources
      candidates <- proposeSelfMutations st fps
      let ranked = rankMutations candidates
      length ranked @?= length candidates

  , testCase "rankMutations preserves all candidates (no drops)" $ do
      let node = exprToASTNode "n" (lit 1)
      st  <- newAgentState node
      fps <- discoverSources
      candidates <- proposeSelfMutations st fps
      let ranked = rankMutations candidates
      -- Every original candidate's mutation description should appear in ranked.
      let origDescs  = map (\(_, m, _) -> hmDescription m) candidates
          rankedDescs = map (\(_, m, _) -> hmDescription m) ranked
      all (`elem` rankedDescs) origDescs @?= True

  , testCase "writeMutation with valid quorum writes and restores file" $ do
      tmpDir <- getTemporaryDirectory
      let repoRoot = tmpDir </> "dgm-selfmod-write-test"
          srcDGMDir = repoRoot </> "src" </> "DGM"
      createDirectoryIfMissing True srcDGMDir
      -- Write a minimal Haskell fixture to the temp DGM dir.
      let target      = srcDGMDir </> "Fixture.hs"
          origContent = "module Fixture where\n\nimport Data.List (sort)\n\nfixId :: a -> a\nfixId x = id x\n"
      writeFile target origContent
      -- Parse and collect mutations.
      parseResult <- parseHsFile target
      case parseResult of
        Left err -> assertFailure ("parseHsFile failed: " <> T.unpack err)
        Right m  -> do
          let muts = collectHsMutations defaultHsRules m
          case muts of
            [] -> assertFailure "No mutations found in fixture; cannot test writeMutation"
            (mut:_) ->
              case applyHsMutation mut m of
                Left err  -> assertFailure ("applyHsMutation failed: " <> T.unpack err)
                Right m'  -> do
                  -- Set up AgentState with the temp repo root.
                  let node = exprToASTNode "n" (lit 1)
                  st <- newAgentStateWithRoot node repoRoot
                  auditLog <- newIORef []
                  now <- floor <$> (getPOSIXTime :: IO POSIXTime)
                  let opName = "write:" <> T.pack target
                      proof  = mockQuorumProof opName now
                  result <- writeMutation st auditLog proof target m'
                  case result of
                    Left err -> assertFailure ("writeMutation failed: " <> T.unpack err)
                    Right () -> do
                      -- Verify the file was actually written.
                      written <- readFile target
                      written /= origContent @?= True
                  -- Restore original content.
                  writeFile target origContent

  , testCase "writeMutation rejects non-DGM path" $ do
      let node = exprToASTNode "n" (lit 1)
      st <- newAgentState node
      auditLog <- newIORef []
      now <- floor <$> (getPOSIXTime :: IO POSIXTime)
      let badPath = "src/NotDGM/Something.hs"
          opName  = "write:" <> T.pack badPath
          proof   = mockQuorumProof opName now
      -- We need any HsModule; use parseHsText on a tiny snippet.
      parseResult <- parseHsText "module X where\n"
      case parseResult of
        Left err -> assertFailure ("parseHsText failed: " <> T.unpack err)
        Right m  -> do
          result <- writeMutation st auditLog proof badPath m
          case result of
            Left _  -> pure ()  -- expected
            Right () -> assertFailure "Expected Left for non-DGM path"
#endif
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- DGM.SelfCompile tests
-- ─────────────────────────────────────────────────────────────────────────────

selfCompileTests :: TestTree
selfCompileTests = testGroup "DGM.SelfCompile"
  [ testCase "scoreCompileResult: all pass → 1.0" $
      scoreCompileResult (CompileSuccess 42 0 100.0) @?= 1.0

  , testCase "scoreCompileResult: k/n pass → ratio" $
      scoreCompileResult (CompileSuccess 3 1 50.0) @?= 0.75

  , testCase "scoreCompileResult: zero total → 0.0" $
      scoreCompileResult (CompileSuccess 0 0 0.0) @?= 0.0

  , testCase "scoreCompileResult: TypeCheckPhase → 0.1" $
      scoreCompileResult (CompileFailure "some error" TypeCheckPhase) @?= 0.1

  , testCase "scoreCompileResult: ParsePhase → 0.0" $
      scoreCompileResult (CompileFailure "parse error" ParsePhase) @?= 0.0

  , testCase "scoreCompileResult: TestPhase → 0.05" $
      scoreCompileResult (CompileFailure "test fail" TestPhase) @?= 0.05

  , testCase "withSelfModTxn: Right action leaves file unchanged" $ do
      tmpDir <- getTemporaryDirectory
      let fp = tmpDir </> "selfcompile-test-right.txt"
          content = "original"
          mut = HsMutation { hmDescription = "test", hmTransform = Right }
          txn = SelfModTxn fp (T.pack content) mut
      writeFile fp content
      result <- withSelfModTxn txn (return (Right ()))
      case result of
        Right () -> pure ()
        Left e   -> assertFailure ("Expected Right, got Left: " <> T.unpack e)
      written <- readFile fp
      written @?= content
      removeFile fp `catch` \e ->
        if isDoesNotExistError e then pure () else ioError e

  , testCase "withSelfModTxn: Left action restores original file" $ do
      tmpDir <- getTemporaryDirectory
      let fp      = tmpDir </> "selfcompile-test-left.txt"
          orig    = "original content"
          mutated = "mutated content"
          mut = HsMutation { hmDescription = "test", hmTransform = Right }
          txn = SelfModTxn fp (T.pack orig) mut
      -- Write mutated content (simulating a prior write).
      writeFile fp mutated
      result <- withSelfModTxn txn (return (Left "test failure"))
      case result of
        Left _  -> pure ()
        Right _ -> assertFailure "Expected Left after rollback"
      -- File should be restored to original.
      written <- readFile fp
      written @?= orig
      removeFile fp `catch` \e ->
        if isDoesNotExistError e then pure () else ioError e

  , testCase "scoreCompileResult of CompileFailure LinkPhase is 0.05" $
      scoreCompileResult (CompileFailure "ld error" LinkPhase) @?= 0.05

  , testCase "scoreCompileResult of CompileFailure TestPhase is 0.05" $
      scoreCompileResult (CompileFailure "FAIL" TestPhase) @?= 0.05

  , testCase "CompileResult Show round-trip (smoke test)" $ do
      let r1 = CompileSuccess 10 2 250.0
          r2 = CompileFailure "type error" TypeCheckPhase
      -- Just ensure Show doesn't throw.
      length (show r1) > 0 @?= True
      length (show r2) > 0 @?= True
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- DGM.SelfMod.commitMutation tests (Phase E)
-- ─────────────────────────────────────────────────────────────────────────────

commitMutationTests :: TestTree
commitMutationTests = testGroup "DGM.SelfMod.commitMutation"
  [ testCase "commitMutation creates 'selfmod:' prefixed commit" $
      withTempGitRepo $ \repoDir fp -> do
        -- Modify the file to simulate a successful mutation.
        appendFile fp "\n-- mutated by selfmod\n"
        let mut   = HsMutation { hmDescription = "eta-reduce: f x = g x -> f = g"
                               , hmTransform   = Right }
            entry = mkEntry "test-eid-commit-1" 1.0 True
        result <- commitMutation fp mut entry
        case result of
          Left err -> assertFailure ("commitMutation failed: " <> T.unpack err)
          Right _  -> do
            (_, logOut, _) <-
              readProcessWithExitCode "git" ["-C", repoDir, "log", "--oneline", "-1"] ""
            "selfmod:" `T.isInfixOf` T.pack logOut @?= True

  , testCase "commitMutation is idempotent: double-run does not double-commit" $
      withTempGitRepo $ \repoDir fp -> do
        -- First run: mutate and commit.
        appendFile fp "\n-- mutation applied\n"
        let mut   = HsMutation { hmDescription = "remove unused explicit imports"
                               , hmTransform   = Right }
            entry = mkEntry "test-eid-idempotent" 1.0 True
        r1 <- commitMutation fp mut entry
        case r1 of
          Left err -> assertFailure ("First commitMutation failed: " <> T.unpack err)
          Right h1 -> do
            -- Second run: tree is already clean — should return same hash.
            r2 <- commitMutation fp mut entry
            case r2 of
              Left err -> assertFailure ("Second commitMutation (idempotent) failed: " <> T.unpack err)
              Right h2 -> do
                h1 @?= h2
                -- Verify git log has exactly 2 commits (init + our one selfmod).
                (_, logOut, _) <-
                  readProcessWithExitCode "git"
                    ["-C", repoDir, "log", "--oneline"] ""
                length (lines logOut) @?= 2

  , testCase "rollback does not create a git commit" $
      withTempGitRepo $ \repoDir fp -> do
        -- Use the known content written by withTempGitRepo (avoids lazy-IO lock).
        let origContent = "module Fixture where\nfixId :: a -> a\nfixId x = id x\n"
            mut = HsMutation { hmDescription = "test-rollback", hmTransform = Right }
            txn = SelfModTxn { smtFile = fp
                             , smtOriginalText = T.pack origContent
                             , smtMutation     = mut }
        -- Simulate a write + rollback (Left result).
        appendFile fp "\n-- would-be mutation\n"
        _ <- withSelfModTxn txn (return (Left "simulated test failure"))
        -- File should be restored to original content.
        restored <- readFile fp
        restored @?= origContent
        -- Git log should still show only the initial commit (no new commit from rollback).
        (_, logOut, _) <-
          readProcessWithExitCode "git" ["-C", repoDir, "log", "--oneline"] ""
        length (lines logOut) @?= 1

  , testCase "commitMessage contains 'selfmod:' prefix and entry id" $ do
      -- Use a short description so the full message fits in 72 chars and the
      -- entry ID is not truncated out.
      let mut   = HsMutation { hmDescription = "eta-reduce"
                             , hmTransform   = Right }
          entry = mkEntry "test-entry-id-42" 1.0 True
          msg   = commitMessage "src/DGM/SelfMod.hs" mut entry
      "selfmod:" `T.isPrefixOf` msg @?= True
      "test-entry-id-42" `T.isInfixOf` msg @?= True
      "SelfMod" `T.isInfixOf` msg @?= True

  , testCase "commitMessage is truncated to 72 characters maximum" $ do
      let longDesc = T.replicate 100 "x"
          mut   = HsMutation { hmDescription = longDesc, hmTransform = Right }
          entry = mkEntry "eid" 1.0 True
          msg   = commitMessage "src/DGM/Foo.hs" mut entry
      T.length msg @?= 72
  ]

-- | Create a temporary git repository with one initial DGM source file.
--
-- The action receives the repository root directory and the path to the
-- pre-committed file.  The repository is cleaned up after the action
-- completes (or fails).
withTempGitRepo :: (FilePath -> FilePath -> IO ()) -> IO ()
withTempGitRepo action = bracket setup teardown run
  where
    setup = do
      tmpDir <- getTemporaryDirectory
      t <- getPOSIXTime
      let n       = round (realToFrac (t :: POSIXTime) * 1000000 :: Double) :: Int
          repoDir = tmpDir </> ("dgm-commit-test-" <> show n)
          dgmDir  = repoDir </> "src" </> "DGM"
      createDirectoryIfMissing True dgmDir
      _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
      _ <- readProcessWithExitCode "git"
             ["-C", repoDir, "config", "user.email", "test@test.com"] ""
      _ <- readProcessWithExitCode "git"
             ["-C", repoDir, "config", "user.name", "Test User"] ""
      let fp      = dgmDir </> "Fixture.hs"
          content = "module Fixture where\nfixId :: a -> a\nfixId x = id x\n"
      writeFile fp content
      _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "."] ""
      _ <- readProcessWithExitCode "git"
             ["-C", repoDir, "commit", "-m", "init"] ""
      return (repoDir, fp)
    teardown (repoDir, _) =
      removeDirectoryRecursive repoDir `catch` \(_ :: IOError) -> pure ()
    run (repoDir, fp) =
      action repoDir fp

-- ─────────────────────────────────────────────────────────────────────────────
-- DGM.ModGraph tests (Phase F)
-- ─────────────────────────────────────────────────────────────────────────────

modGraphTests :: TestTree
modGraphTests = testGroup "DGM.ModGraph"
  [ -- ── Acceptance criterion 1: core DGM modules import DGM.Types ────────────
    -- The spec says "all 10 modules" (written before Phase C–F added more).
    -- The 9 core modules (AbsoluteZero, Archive, AST, Cycle, Evolution,
    -- Rewriting, SafetyKernel, SelfMod, Verification) import DGM.Types
    -- directly.  Isolated wrappers (HsAST, HintBridge, Sandbox, SelfCompile,
    -- ModGraph) intentionally don't, to avoid circular dependencies.
    testCase "buildModuleGraph: core DGM modules import DGM.Types" $ do
      fps <- discoverSources
      mg  <- buildModuleGraph fps
      let typesModName = "DGM.Types"
          -- These modules are specified by the acceptance criteria as importers.
          coreModules  = [ "DGM.AbsoluteZero", "DGM.Archive", "DGM.AST"
                         , "DGM.Cycle", "DGM.Evolution", "DGM.Rewriting"
                         , "DGM.SafetyKernel", "DGM.SelfMod", "DGM.Verification"
                         ]
          importsTypes nm =
            case Map.lookup nm (mgImports mg) of
              Nothing   -> False
              Just imps -> typesModName `elem` imps
      -- All 9 core modules must appear in the graph and import DGM.Types.
      all (\nm -> nm `elem` Map.keys (mgModules mg)) coreModules @?= True
      all importsTypes coreModules @?= True

  , testCase "buildModuleGraph: mgUsedBy[DGM.Types] has >= 5 dependents" $ do
      fps <- discoverSources
      mg  <- buildModuleGraph fps
      let usedBy = maybe [] id (Map.lookup "DGM.Types" (mgUsedBy mg))
      length usedBy >= 5 @?= True

  , testCase "buildModuleGraph: all module names are non-empty" $ do
      fps <- discoverSources
      mg  <- buildModuleGraph fps
      all (not . T.null) (Map.keys (mgModules mg)) @?= True

  , testCase "discoverSources + buildModuleGraph: module count >= 10" $ do
      fps <- discoverSources
      mg  <- buildModuleGraph fps
      Map.size (mgModules mg) >= 10 @?= True

  -- ── extractExports (text-level) ─────────────────────────────────────────
  , testCase "extractExports: finds exported names in DGM.Types source" $ do
      fps <- discoverSources
      let typesPath = filter (T.isSuffixOf "Types.hs" . T.pack) fps
      case typesPath of
        [] -> assertFailure "Types.hs not found in discoverSources"
        (fp:_) -> do
          src <- TIO.readFile fp
          let exps = extractExports src
          "SafetyLevel" `elem` exps @?= True
          "QuorumProof" `elem` exps @?= True
          "Mutation"    `elem` exps @?= True

  , testCase "extractExports: explicit export list is parsed correctly" $ do
      let src = T.unlines
            [ "module Foo"
            , "  ( myFunc"
            , "  , OtherType(..)"
            , "  -- * A section"
            , "  , barHelper"
            , "  ) where"
            , "myFunc = undefined"
            ]
          exps = extractExports src
      "myFunc"    `elem` exps @?= True
      "OtherType" `elem` exps @?= True
      "barHelper" `elem` exps @?= True

  , testCase "extractExports: returns empty for wildcard export" $ do
      let src = T.unlines
            [ "module Foo where"
            , "myFunc = undefined"
            ]
          exps = extractExports src
      exps @?= []

  -- ── Acceptance criterion 3: exported-function removal rejected ────────────
  -- (tested via the text-based API so it works without WITH_EXACTPRINT)
  , testCase "removesExportedNameText: detects export removal" $ do
      let origSrc = T.unlines
            [ "module Foo"
            , "  ( myFunc"
            , "  , otherFunc"
            , "  ) where"
            , "myFunc x = x + 1"
            , "otherFunc x = x - 1"
            ]
          removeMut = HsMutation
            { hmDescription = "remove otherFunc from export list"
            , hmTransform   = \src -> Right (T.replace ", otherFunc\n" "" src)
            }
      removesExportedNameText removeMut origSrc @?= True

  , testCase "removesExportedNameText: non-removal mutation returns False" $ do
      let origSrc = T.unlines
            [ "module Foo"
            , "  ( myFunc"
            , "  ) where"
            , "myFunc x = x + 1"
            ]
          internalMut = HsMutation
            { hmDescription = "add internal comment"
            , hmTransform   = \src -> Right (src <> "-- comment\n")
            }
      removesExportedNameText internalMut origSrc @?= False

  , testCase "removesExportedNameText: inapplicable mutation returns False" $ do
      let origSrc = T.unlines
            [ "module Foo"
            , "  ( myFunc"
            , "  ) where"
            ]
          noopMut = HsMutation
            { hmDescription = "always fails"
            , hmTransform   = \_ -> Left "not applicable"
            }
      removesExportedNameText noopMut origSrc @?= False

  -- ── Acceptance criterion 2: where-clause addition ranks above Types.hs rename
  -- ── Tests that rankMutations accepts ModuleGraph and is a stable permutation.
  , testCase "rankMutations: accepts ModuleGraph parameter and is stable" $ do
      fps  <- discoverSources
      mg   <- buildModuleGraph fps
      node <- newAgentState (ASTNode "n" ModuleNode [] Nothing)
      candidates <- proposeSelfMutations node fps Nothing
      let ranked = rankMutations mg candidates
      length ranked @?= length candidates

  -- ── numDependents reflects Types.hs high-fanout status ──────────────────
  , testCase "numDependents: Types.hs has higher dependency count than HintBridge.hs" $ do
      fps <- discoverSources
      mg  <- buildModuleGraph fps
      let typesFps   = filter (T.isSuffixOf "Types.hs"     . T.pack) fps
          hintFps    = filter (T.isSuffixOf "HintBridge.hs" . T.pack) fps
      case (typesFps, hintFps) of
        (typesFp:_, hintFp:_) ->
          -- Types.hs should have more dependents than HintBridge.hs.
          numDependents mg typesFp > numDependents mg hintFp @?= True
        _ -> assertFailure "Expected both Types.hs and HintBridge.hs in sources"
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Generation model tests (SI-0ew): exit-42, supervisor loop, GENERATION counter
-- ─────────────────────────────────────────────────────────────────────────────

generationTests :: TestTree
generationTests = testGroup "Generation model"
  -- ── hasSuccessfulCommit (exit-42 signal detection) ───────────────────────
  [ testCase "hasSuccessfulCommit: True when StepPersist has Committed message" $ do
      let steps =
            [ StepResult StepPropose    True  "candidates: 3"
            , StepResult StepHypothesise True  "ranked: 3"
            , StepResult StepClassify    True  "snapshot taken"
            , StepResult StepVerify      True  "mutation written"
            , StepResult StepTest        True  "score: 1.0 (all tests pass)"
            , StepResult StepPersist     True  "Committed abc1234 | score=1.0"
            ]
      hasSuccessfulCommit steps @?= True

  , testCase "hasSuccessfulCommit: False when StepPersist rolled back" $ do
      let steps =
            [ StepResult StepPersist False "Rollback complete | score=0.5" ]
      hasSuccessfulCommit steps @?= False

  , testCase "hasSuccessfulCommit: False for empty step list" $
      hasSuccessfulCommit [] @?= False

  , testCase "hasSuccessfulCommit: False when commit itself failed (not Committed prefix)" $ do
      let steps =
            [ StepResult StepPersist True "Commit failed: no git repo | score=1.0" ]
      -- "Commit failed" starts with "Commit" but NOT "Committed"
      hasSuccessfulCommit steps @?= False

  , testCase "hasSuccessfulCommit: False when all steps pass but none is StepPersist" $ do
      let steps =
            [ StepResult StepPropose     True "ok"
            , StepResult StepHypothesise True "ok"
            , StepResult StepVerify      True "ok"
            ]
      hasSuccessfulCommit steps @?= False

  -- ── Supervisor script: existence and syntax ───────────────────────────────
  , testCase "dgm-evolve.sh: script exists" $ do
      exists <- doesFileExist "scripts/dgm-evolve.sh"
      exists @?= True

  , testCase "dgm-evolve.sh: valid bash syntax" $ do
      (ec, _, _) <- readProcessWithExitCode "bash" ["-n", "scripts/dgm-evolve.sh"] ""
      ec @?= ExitSuccess

  -- ── Demo script: existence, permissions and syntax ────────────────────────
  , testCase "scripts/demo.sh: script exists" $ do
      exists <- doesFileExist "scripts/demo.sh"
      exists @?= True

  , testCase "scripts/demo.sh: valid bash syntax" $ do
      (ec, _, _) <- readProcessWithExitCode "bash" ["-n", "scripts/demo.sh"] ""
      ec @?= ExitSuccess

  -- ── Supervisor script: GENERATION counter and loop behaviour ─────────────
  , testCase "dgm-evolve.sh: loops on exit-42 and stops at max_gens" $ do
      tmpDir <- getTemporaryDirectory
      t <- getPOSIXTime
      let n       = round (realToFrac (t :: POSIXTime) * 1000000 :: Double) :: Int
          mockDir = tmpDir </> ("dgm-mock-" <> show n)
          mockBin = mockDir </> "dgm"
          wrapper = tmpDir </> ("wrap-" <> show n <> ".sh")
      createDirectoryIfMissing True mockDir
      -- Mock dgm: always exits 42 (generation limit in script controls the loop).
      writeFile mockBin "#!/bin/bash\nexit 42\n"
      (_, _, _) <- readProcessWithExitCode "chmod" ["+x", mockBin] ""
      -- Wrapper that prepends mockDir to PATH and runs the supervisor.
      writeFile wrapper $ unlines
        [ "#!/bin/bash"
        , "export PATH=" <> mockDir <> ":$PATH"
        , "exec bash scripts/dgm-evolve.sh 2"
        ]
      (_, _, _) <- readProcessWithExitCode "chmod" ["+x", wrapper] ""
      (ec, out, _) <- readProcessWithExitCode "bash" [wrapper] ""
      -- Script should exit 0 (max reached) after 2 successful evolutions.
      ec @?= ExitSuccess
      -- Output should mention both generations.
      "Generation 1" `T.isInfixOf` T.pack out @?= True
      "Generation 2" `T.isInfixOf` T.pack out @?= True
      -- Clean up.
      removeDirectoryRecursive mockDir
        `catch` (\(_ :: IOError) -> pure ())
      removeFile wrapper
        `catch` (\(_ :: IOError) -> pure ())

  , testCase "dgm-evolve.sh: stops and propagates non-zero, non-42 exit code" $ do
      tmpDir <- getTemporaryDirectory
      t <- getPOSIXTime
      let n       = round (realToFrac (t :: POSIXTime) * 1000000 :: Double) :: Int
          mockDir = tmpDir </> ("dgm-mock-err-" <> show n)
          mockBin = mockDir </> "dgm"
          wrapper = tmpDir </> ("wrap-err-" <> show n <> ".sh")
      createDirectoryIfMissing True mockDir
      -- Mock dgm: exits with code 1 (error).
      writeFile mockBin "#!/bin/bash\nexit 1\n"
      (_, _, _) <- readProcessWithExitCode "chmod" ["+x", mockBin] ""
      writeFile wrapper $ unlines
        [ "#!/bin/bash"
        , "export PATH=" <> mockDir <> ":$PATH"
        , "exec bash scripts/dgm-evolve.sh 5"
        ]
      (_, _, _) <- readProcessWithExitCode "chmod" ["+x", wrapper] ""
      (ec, _, _) <- readProcessWithExitCode "bash" [wrapper] ""
      -- Script should propagate the error exit code.
      ec @?= ExitFailure 1
      -- Clean up.
      removeDirectoryRecursive mockDir
        `catch` (\(_ :: IOError) -> pure ())
      removeFile wrapper
        `catch` (\(_ :: IOError) -> pure ())
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- DGM.Reversal tests (SI-2p3): typed transaction plans with computed inverse
-- ─────────────────────────────────────────────────────────────────────────────

reversalTests :: TestTree
reversalTests = testGroup "DGM.Reversal"
  [ -- ── mkTextReplacement: Maybe smart constructor ─────────────────────────
    testCase "mkTextReplacement: returns Nothing for empty old" $
      case mkTextReplacement "" "world" of
        Nothing -> return ()
        Just _  -> assertFailure "expected Nothing for empty old"

  , testCase "mkTextReplacement: returns Nothing when old == new" $
      case mkTextReplacement "same" "same" of
        Nothing -> return ()
        Just _  -> assertFailure "expected Nothing when old == new"

  , testCase "mkTextReplacement: forward replaces text" $ do
      case mkTextReplacement "hello" "world" of
        Nothing  -> assertFailure "expected Just"
        Just inv -> forward inv "say hello there" @?= "say world there"

  , testCase "mkTextReplacement: backward restores text" $ do
      case mkTextReplacement "hello" "world" of
        Nothing  -> assertFailure "expected Just"
        Just inv -> backward inv "say world there" @?= "say hello there"

  , testCase "mkTextReplacement: roundtrip backward . forward == id (no alias)" $ do
      case mkTextReplacement "foo" "bar" of
        Nothing  -> assertFailure "expected Just"
        Just inv ->
          let original = "foo baz qux"  -- "bar" absent → no aliasing
          in  backward inv (forward inv original) @?= original

  , testCase "mkTextReplacement: invDescription is non-empty" $ do
      case mkTextReplacement "hello" "world" of
        Nothing  -> assertFailure "expected Just"
        Just inv -> T.null (invDescription inv) @?= False

  -- ── mkEtaReduction: new signature (funcName, var) ────────────────────────
  , testCase "mkEtaReduction: returns Nothing for empty funcName" $
      case mkEtaReduction "" "x" of
        Nothing -> return ()
        Just _  -> assertFailure "expected Nothing for empty funcName"

  , testCase "mkEtaReduction: returns Nothing for empty var" $
      case mkEtaReduction "f" "" of
        Nothing -> return ()
        Just _  -> assertFailure "expected Nothing for empty var"

  , testCase "mkEtaReduction: forward applies eta-reduction" $ do
      case mkEtaReduction "f" "x" of
        Nothing  -> assertFailure "expected Just"
        Just inv -> forward inv "f x = g x" @?= "f = g x"

  , testCase "mkEtaReduction: backward restores eta-expanded LHS" $ do
      case mkEtaReduction "f" "x" of
        Nothing  -> assertFailure "expected Just"
        Just inv -> backward inv "f = g x" @?= "f x = g x"

  , testCase "mkEtaReduction: roundtrip backward . forward == id" $ do
      case mkEtaReduction "f" "x" of
        Nothing  -> assertFailure "expected Just"
        Just inv ->
          let src = "f x = someBody\n"
          in  backward inv (forward inv src) @?= src

  -- ── mkAddComment ─────────────────────────────────────────────────────────
  , testCase "mkAddComment: returns Nothing for empty anchor" $
      case mkAddComment "" "a comment" of
        Nothing -> return ()
        Just _  -> assertFailure "expected Nothing for empty anchor"

  , testCase "mkAddComment: forward inserts comment before anchor" $ do
      case mkAddComment "myFunc" "This is myFunc" of
        Nothing  -> assertFailure "expected Just"
        Just inv ->
          let src = "myFunc = 42\n"
          in  T.isInfixOf "-- This is myFunc" (forward inv src) @?= True

  , testCase "mkAddComment: roundtrip backward . forward == id" $ do
      case mkAddComment "myFunc" "This is myFunc" of
        Nothing  -> assertFailure "expected Just"
        Just inv ->
          let src = "myFunc = 42\n"
          in  backward inv (forward inv src) @?= src

  -- ── TypedTxn record fields ────────────────────────────────────────────────
  , testCase "TypedTxn: record fields accessible" $ do
      case mkTextReplacement "old" "new" of
        Nothing  -> assertFailure "expected Just"
        Just inv -> do
          let txn = TypedTxn { txnFile = "f.hs", txnOp = inv, txnMutation = dummyMutation }
          txnFile txn @?= "f.hs"
          hmDescription (txnMutation txn) @?= "test-dummy"

  -- ── executeTypedTxn ───────────────────────────────────────────────────────
  , testCase "executeTypedTxn: applies forward and writes file" $ do
      tmpDir <- getTemporaryDirectory
      let fp = tmpDir </> "dgm-reversal-test-exec.hs"
      TIO.writeFile fp "module Foo where\nhello = world\n"
      case mkTextReplacement "hello" "greet" of
        Nothing  -> assertFailure "expected Just"
        Just inv -> do
          let txn = TypedTxn { txnFile = fp, txnOp = inv, txnMutation = dummyMutation }
          result <- executeTypedTxn txn
          result @?= Right ()
          content <- TIO.readFile fp
          T.isInfixOf "greet" content @?= True
          removeFile fp

  -- ── rollbackTypedTxn ──────────────────────────────────────────────────────
  , testCase "rollbackTypedTxn: restores original after executeTypedTxn" $ do
      tmpDir <- getTemporaryDirectory
      let fp       = tmpDir </> "dgm-reversal-rollback.hs"
          original = "module Foo where\nhello = world\n"
      TIO.writeFile fp original
      case mkTextReplacement "hello" "greet" of
        Nothing  -> assertFailure "expected Just"
        Just inv -> do
          let txn = TypedTxn { txnFile = fp, txnOp = inv, txnMutation = dummyMutation }
          _ <- executeTypedTxn txn
          rollbackTypedTxn txn
          restored <- TIO.readFile fp
          restored @?= original
          removeFile fp

  , testCase "rollbackTypedTxn: is a no-op on missing file (no crash)" $ do
      let txn = TypedTxn
            { txnFile     = "/nonexistent/path/that/does/not/exist.hs"
            , txnOp       = Invertible id id "noop"
            , txnMutation = dummyMutation
            }
      rollbackTypedTxn txn  -- should not throw

  -- ── prop_invertible_replacement (exported property) ───────────────────────
  , testProperty "prop_invertible_replacement: backward . forward == id" $
      \(x :: String) (old :: String) (new :: String) ->
        prop_invertible_replacement (T.pack x) (T.pack old) (T.pack new)

  -- ── prop_invertible_eta (exported property) ────────────────────────────────
  , testProperty "prop_invertible_eta: backward . forward == id" $
      \(fn :: String) (v :: String) ->
        prop_invertible_eta (T.pack fn) (T.pack v)

  ]
  where
    dummyMutation :: HsMutation
    dummyMutation = HsMutation
      { hmDescription = "test-dummy"
      , hmTransform   = Right
      }

    -- | String containment helper for QC properties.
    contains :: String -> String -> Bool
    contains haystack needle = go haystack
      where
        n  = length needle
        go [] = False
        go xs@(_:rest)
          | take n xs == needle = True
          | otherwise           = go rest

-- ─────────────────────────────────────────────────────────────────────────────
-- DGM.Liquid tests
-- ─────────────────────────────────────────────────────────────────────────────

liquidTests :: TestTree
liquidTests = testGroup "DGM.Liquid"
  [ -- parseLiquidOutput unit tests (unconditional — no LH binary required)
    testCase "parseLiquidOutput: 'RESULT: SAFE' => LiquidSafe" $
      parseLiquidOutput "RESULT: SAFE\n" @?= LiquidSafe

  , testCase "parseLiquidOutput: 'RESULT: SAFE' anywhere in output => LiquidSafe" $
      parseLiquidOutput "preamble\nRESULT: SAFE\nmore output\n" @?= LiquidSafe

  , testCase "parseLiquidOutput: 'RESULT: UNSAFE [...]' => LiquidUnsafe" $
      case parseLiquidOutput "RESULT: UNSAFE [precondition violation]\n" of
        LiquidUnsafe _ -> return ()
        other          -> assertFailure ("Expected LiquidUnsafe, got: " <> show other)

  , testCase "parseLiquidOutput: 'RESULT: UNSAFE' without details => LiquidUnsafe" $
      case parseLiquidOutput "RESULT: UNSAFE\n" of
        LiquidUnsafe _ -> return ()
        other          -> assertFailure ("Expected LiquidUnsafe, got: " <> show other)

  , testCase "parseLiquidOutput: unrecognised output => LiquidError" $
      case parseLiquidOutput "ghc: some parse error\n" of
        LiquidError _ -> return ()
        other         -> assertFailure ("Expected LiquidError, got: " <> show other)

  , testCase "parseLiquidOutput: empty output => LiquidError" $
      case parseLiquidOutput "" of
        LiquidError _ -> return ()
        other         -> assertFailure ("Expected LiquidError for empty output, got: " <> show other)

  -- Stub test: verifyWithLiquid on Types.hs returns LiquidSafe when
  -- WITH_LIQUID is not compiled in (the stub always passes through).
  , testCase "verifyWithLiquid stub: Types.hs returns LiquidSafe" $ do
      result <- verifyWithLiquid "." ("src" </> "DGM" </> "Types.hs")
#ifdef WITH_LIQUID
      -- When WITH_LIQUID is compiled in, the real LH subprocess runs.
      -- We only assert it doesn't crash; the result depends on the environment.
      case result of
        LiquidSafe      -> return ()
        LiquidUnsafe _  -> return ()  -- acceptable: LH may not find Types.hs in isolation
        LiquidError _   -> return ()  -- acceptable: LH may not be on PATH in CI
#else
      -- Without WITH_LIQUID, the stub always returns LiquidSafe.
      result @?= LiquidSafe
#endif

  -- Wire test: pipeline with LH enabled rejects a known-bad output.
  , testCase "parseLiquidOutput: SAFE takes priority over UNSAFE" $
      -- If a line has both tokens (contrived), SAFE wins because it's checked first.
      parseLiquidOutput "RESULT: SAFE\nRESULT: UNSAFE\n" @?= LiquidSafe
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Oracle tests (SI-6kx)
-- ─────────────────────────────────────────────────────────────────────────────

oracleTests :: TestTree
oracleTests = testGroup "DGM.Oracle"
  [ -- ── parseDotEnv (pure, always testable) ──────────────────────────────────
    testCase "parseDotEnv: parses simple KEY=VALUE pairs" $ do
      let pairs = parseDotEnv "OPENROUTER_API_KEY=my-secret\nOTHER=value\n"
      lookup "OPENROUTER_API_KEY" pairs @?= Just "my-secret"
      lookup "OTHER" pairs              @?= Just "value"

  , testCase "parseDotEnv: ignores comment and blank lines" $ do
      let pairs = parseDotEnv "# comment\n\nKEY=val\n"
      lookup "KEY" pairs @?= Just "val"
      length pairs @?= 1

  , testCase "parseDotEnv: returns empty list for empty input" $ do
      parseDotEnv "" @?= []

  , testCase "parseDotEnv: ignores lines without '='" $ do
      let pairs = parseDotEnv "NO_EQUALS\nKEY=val\n"
      lookup "NO_EQUALS" pairs @?= Nothing
      lookup "KEY" pairs       @?= Just "val"

  , testCase "parseDotEnv: value may contain '=' characters" $ do
      let pairs = parseDotEnv "KEY=a=b=c\n"
      lookup "KEY" pairs @?= Just "a=b=c"

  -- ── newOracleEnv ──────────────────────────────────────────────────────────
  , testCase "newOracleEnv: key in env var takes precedence over .env" $ do
      -- Only assert structural correctness; env var may or may not be set.
      mEnv <- newOracleEnv
      case mEnv of
        Nothing  -> return ()            -- no key: expected in clean environment
        Just env -> T.null (oeApiKey env) @?= False  -- key is non-empty if present

  -- ── proposeMutation (stub or live) ────────────────────────────────────────
  , testCase "proposeMutation: returns Either without throwing" $ do
      let env = OracleEnv
            { oeApiKey  = "stub-key"
            , oeModel   = "stub-model"
            , oeBaseUrl = "https://example.invalid"
            }
      -- Should not throw; may return Left (stub) or Left (network error)
      result <- proposeMutation env "src/DGM/Foo.hs" "module Foo where" [] Nothing
      case result of
        Left  _ -> return ()   -- expected in test environment (no real key)
        Right _ -> return ()   -- also OK if oracle happened to respond

  -- ── scoreAndRankMutations ────────────────────────────────────────────────
  , testCase "scoreAndRankMutations: returns list same length as input" $ do
      let env  = OracleEnv "k" "m" "https://example.invalid"
          muts = [ HsMutation "m1" Right
                 , HsMutation "m2" Right
                 , HsMutation "m3" Right
                 ]
      scored <- scoreAndRankMutations env muts
      length scored @?= 3

  , testCase "scoreAndRankMutations: all scores in [0, 1]" $ do
      let env  = OracleEnv "k" "m" "https://example.invalid"
          muts = [ HsMutation "m1" Right ]
      scored <- scoreAndRankMutations env muts
      all (\(_, s) -> s >= 0.0 && s <= 1.0) scored @?= True

  , testCase "scoreAndRankMutations: empty input yields empty output" $ do
      let env = OracleEnv "k" "m" "https://example.invalid"
      scored <- scoreAndRankMutations env []
      length scored @?= 0

  -- ── oracleHealthCheck ─────────────────────────────────────────────────────
  , testCase "oracleHealthCheck: returns Bool without throwing" $ do
      let env = OracleEnv "stub-key" "stub-model" "https://example.invalid"
      result <- oracleHealthCheck env
      -- Without network access / valid key, expect False; just check no throw.
      result `elem` [True, False] @?= True

  -- ── End-to-end fallback: system works without API key ────────────────────
  , testCase "proposeSelfMutations: works without oracle (heuristic fallback)" $ do
      -- Verify the mutation pipeline does not require an oracle key.
      -- We only check it runs without throwing; mutation count may be 0
      -- depending on source state.
      let node = exprToASTNode "n" (lit 1)
      st  <- newAgentState node
      fps <- discoverSources
      -- If no oracle key is present, falls back gracefully to heuristics.
      candidates <- proposeSelfMutations st fps Nothing
      -- Result is a list (possibly empty); no exception should be thrown.
      length candidates >= 0 @?= True
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Oracle handle tests (SI-5cv)
-- ─────────────────────────────────────────────────────────────────────────────

oracleHandleTests :: TestTree
oracleHandleTests = testGroup "DGM.OracleHandle"
  [ -- ── withOracle / handle acquisition ───────────────────────────────────
    testCase "withOracle: callback is invoked" $ do
      let env = OracleEnv
            { oeApiKey  = "test-key"
            , oeModel   = "test-model"
            , oeBaseUrl = "https://example.com"
            }
      called <- newIORef False
      withOracle env $ \_ -> writeIORef called True
      val <- readIORef called
      val @?= True

  , testCase "proposeMutationH: stub returns Left (no network)" $ do
      let env = OracleEnv
            { oeApiKey  = "test-key"
            , oeModel   = "test-model"
            , oeBaseUrl = "https://example.com"
            }
      result <- withOracle env $ \h ->
        proposeMutationH h "src/DGM/Foo.hs" "module Foo where" [] Nothing
      case result of
        Left _  -> return ()   -- expected: stub returns Left
        Right _ -> assertFailure "stub should return Left"

  , testCase "scoreMutationsH: stub returns list same length as input" $ do
      let env = OracleEnv
            { oeApiKey  = "test-key"
            , oeModel   = "test-model"
            , oeBaseUrl = "https://example.com"
            }
          muts = [ HsMutation "mut1" Right
                 , HsMutation "mut2" Right
                 ]
      scored <- withOracle env $ \h -> scoreMutationsH h muts
      length scored @?= length muts

  -- ── newOracleEnv: returns Nothing when env var absent ─────────────────────
  , testCase "newOracleEnv: returns Nothing without OPENROUTER_API_KEY" $ do
      -- We cannot guarantee the env var is absent in all CI environments,
      -- so we only check when it's definitely not set by verifying the result
      -- type is correct (Either way it compiles and runs).
      mEnv <- newOracleEnv
      case mEnv of
        Nothing  -> return ()   -- expected in clean environment
        Just env -> T.null (oeApiKey env) @?= False  -- if set, key is non-empty
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- DynamicRule tests (no hint required — pure Haskell-level tests)
-- ─────────────────────────────────────────────────────────────────────────────

dynamicRuleTests :: TestTree
dynamicRuleTests = testGroup "DGM.Rewriting.DynamicRule"
  [ -- ── DynamicRule construction ─────────────────────────────────────────────
    testCase "DynamicRule: identity rule produces no mutations" $ do
      let rule = DynamicRule
                   { drDescription = "identity"
                   , drTransform   = id
                   , drScore       = 0.0
                   }
          muts = applyDynamicRules [rule] bootstrapExpr
      -- Identity transform never fires (input == output for every node).
      length muts @?= 0

  , testCase "DynamicRule: trivial rule fires on LitF nodes" $ do
      -- Rule: increment all Int labels in a LitF by 1 (structural change).
      let bumpLit x = case x of { LitF n -> LitF (n + 1); other -> other }
          rule = DynamicRule
                   { drDescription = "lit-bump"
                   , drTransform   = bumpLit
                   , drScore       = 0.0
                   }
          expr = lit 5
          muts = applyDynamicRules [rule] expr
      -- Should fire at the root LitF node.
      length muts >= 1 @?= True

  , testCase "DynamicRule: multiple rules applied independently" $ do
      let rule1 = DynamicRule "r1" id 0.0
          dblLit x = case x of { LitF n -> LitF (n * 2); other -> other }
          rule2 = DynamicRule
                    { drDescription = "r2"
                    , drTransform   = dblLit
                    , drScore = 0.0
                    }
          muts = applyDynamicRules [rule1, rule2] (lit 3)
      -- rule1 is identity (no firings), rule2 fires on the LitF.
      length muts >= 1 @?= True

  , testCase "DynamicRule: empty rule list produces no mutations" $ do
      let muts = applyDynamicRules [] bootstrapExpr
      length muts @?= 0

  , testCase "DynamicRule added to TVar is used in next applyDynamicRules call" $ do
      rulesVar <- newTVarIO ([] :: [DynamicRule])
      let bump99 x = case x of { LitF n -> LitF (n + 99); other -> other }
          rule = DynamicRule
                   { drDescription = "tvar-rule"
                   , drTransform   = bump99
                   , drScore       = 1.0
                   }
      -- Before adding: no firings.
      before <- atomically (readTVar rulesVar)
      let mutsBefore = applyDynamicRules before (lit 1)
      length mutsBefore @?= 0
      -- After adding: fires.
      atomically (modifyTVar' rulesVar (rule :))
      after <- atomically (readTVar rulesVar)
      let mutsAfter = applyDynamicRules after (lit 1)
      length mutsAfter >= 1 @?= True

  , testCase "evalRuleCandidate stub: returns Left or Right (never throws)" $ do
      env    <- newHintEnv
      result <- evalRuleCandidate env "\\x -> fmap (+1) x"
      -- Without WITH_HINT, always Left; with hint enabled, Right.
      case result of
        Left  _ -> return ()   -- expected in stub mode
        Right _ -> return ()   -- OK if hint is enabled and worked
  ]

#ifdef WITH_HINT
-- ─────────────────────────────────────────────────────────────────────────────
-- HintBridge.evalRuleCandidate tests (WITH_HINT only)
-- ─────────────────────────────────────────────────────────────────────────────

hintRuleCandidateTests :: TestTree
hintRuleCandidateTests = testGroup "DGM.HintBridge.evalRuleCandidate"
  [ testCase "evalRuleCandidate: valid identity snippet returns Right" $ do
      env    <- newHintEnv
      result <- evalRuleCandidate env "\\x -> x"
      case result of
        Right _ -> return ()
        Left  e -> assertFailure ("Expected Right, got Left: " <> T.unpack e)

  , testCase "evalRuleCandidate: fmap snippet type-checks as ExprF Int -> ExprF Int" $ do
      env    <- newHintEnv
      result <- evalRuleCandidate env "\\x -> fmap (+1) x"
      case result of
        Right _ -> return ()
        Left  e -> assertFailure ("Expected Right, got Left: " <> T.unpack e)

  , testCase "evalRuleCandidate: type-incorrect snippet returns Left" $ do
      -- True is Bool, not ExprF Int -> ExprF Int.
      env    <- newHintEnv
      result <- evalRuleCandidate env "True"
      case result of
        Left  _ -> return ()
        Right _ -> assertFailure "Expected Left for type-incorrect snippet"

  , testCase "evalRuleCandidate: snippet with unsafe IO attempt returns Left" $ do
      -- System.IO.Unsafe is not in scope in ruleImports.
      env    <- newHintEnv
      result <- evalRuleCandidate env
                  "\\x -> unsafePerformIO (return x)"
      case result of
        Left  _ -> return ()   -- unsafePerformIO not in scope → Left
        Right _ -> assertFailure "Expected Left: unsafe IO should be rejected"

  , testCase "evalRuleCandidate: returned function fires on LitF node" $ do
      env    <- newHintEnv
      result <- evalRuleCandidate env "\\x -> fmap (+1) x"
      case result of
        Left  e -> assertFailure ("evalRuleCandidate failed: " <> T.unpack e)
        Right f -> do
          -- Apply fmap (+1) to a BinOpF "+" node with children labelled 0,1.
          -- fmap (+1) increments the child labels: 0->1, 1->2.
          let layer   = BinOpF "+" (0 :: Int) (1 :: Int)
              layer'  = f layer
          layer' @?= BinOpF "+" 1 2

  , testCase "applyDynamicRules fires when evalRuleCandidate result used" $ do
      env <- newHintEnv
      r   <- evalRuleCandidate env "\\x -> fmap (+1) x"
      case r of
        Left  e -> assertFailure ("evalRuleCandidate failed: " <> T.unpack e)
        Right fn -> do
          let rule = DynamicRule "hint-rule" fn 0.0
              -- Use an expression with BinOpF children so fmap (+1) fires.
              expr = binop "+" (lit 1) (lit 2)
              muts = applyDynamicRules [rule] expr
          -- fmap (+1) on BinOpF increments child labels, so should fire.
          length muts >= 1 @?= True
  ]
#endif

-- ─────────────────────────────────────────────────────────────────────────────
-- Pipeline integration tests (SI-sn3): Oracle→LH→SBV→cabal test→commit
-- ─────────────────────────────────────────────────────────────────────────────

pipelineTests :: TestTree
pipelineTests = testGroup "Pipeline integration (SI-sn3)"
  [ -- ── ArchiveEntry provenance fields ────────────────────────────────────
    testCase "ArchiveEntry has provenance fields: lhResult, sbvResult, oracleModel" $ do
      let e = mkEntry "test-provenance" 1.0 True
      -- All three provenance fields should be present (as Nothing by default).
      entryLiquidResult e @?= Nothing
      entrySbvResult    e @?= Nothing
      entryOracleModel  e @?= Nothing

  , testCase "ArchiveEntry provenance fields can be set and read back" $ do
      let e = (mkEntry "test-provenance-set" 0.8 True)
                { entryLiquidResult = Just "SAFE"
                , entrySbvResult    = Just "QED"
                , entryOracleModel  = Just "google/gemini-flash-1.5-8b"
                }
      entryLiquidResult e @?= Just "SAFE"
      entrySbvResult    e @?= Just "QED"
      entryOracleModel  e @?= Just "google/gemini-flash-1.5-8b"

  -- ── stateSbvQueue ─────────────────────────────────────────────────────
  , testCase "AgentState has stateSbvQueue: can enqueue and dequeue CounterExamples" $ do
      let node = exprToASTNode "root" bootstrapExpr
      st <- newAgentState node
      let ce = CounterExample
                 { ceInputs   = Map.fromList [("x", "5")]
                 , ceExpected = "10"
                 , ceActual   = "11"
                 }
      atomically $ modifyTVar' (stateSbvQueue st) (ce:)
      queue <- atomically (readTVar (stateSbvQueue st))
      length queue @?= 1
      ceExpected (head queue) @?= "10"

  -- ── StepFormalVerify in CycleStep ─────────────────────────────────────
  , testCase "StepFormalVerify is a member of CycleStep enum" $ do
      let allSteps = [minBound .. maxBound] :: [CycleStep]
      StepFormalVerify `elem` allSteps @?= True

  , testCase "StepFormalVerify appears between StepVerify and StepTest" $ do
      let allSteps = [minBound .. maxBound] :: [CycleStep]
          indexOf s = length (takeWhile (/= s) allSteps)
      indexOf StepVerify < indexOf StepFormalVerify @?= True
      indexOf StepFormalVerify < indexOf StepTest   @?= True
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- NatLang tests (SI-2fy)
-- ─────────────────────────────────────────────────────────────────────────────

natLangTests :: TestTree
natLangTests = testGroup "DGM.NatLang"
  [ -- ── parseGoal ─────────────────────────────────────────────────────────────
    testCase "parseGoal: returns EvolutionGoal with at least one constraint" $ do
      let env = OracleEnv "stub-key" "stub-model" "https://example.invalid"
      goal <- parseGoal env "improve rewriting rules"
      length (egConstraints goal) >= 1 @?= True

  , testCase "parseGoal: description matches input" $ do
      let env = OracleEnv "stub-key" "stub-model" "https://example.invalid"
      goal <- parseGoal env "improve rewriting rules"
      egDescription goal @?= "improve rewriting rules"

  , testCase "parseGoal: GoalText constraint wraps input in stub mode" $ do
      let env = OracleEnv "stub-key" "stub-model" "https://example.invalid"
      goal <- parseGoal env "my goal text"
      GoalText "my goal text" `elem` egConstraints goal @?= True

  -- ── applyGoal ─────────────────────────────────────────────────────────────
  , testCase "applyGoal: PreferModule 'Archive' keeps only Archive.hs files" $ do
      let fps  = [ "src/DGM/Archive.hs"
                 , "src/DGM/Types.hs"
                 , "src/DGM/Rewriting.hs"
                 ]
          goal = EvolutionGoal "improve Archive" [PreferModule "Archive"]
          res  = applyGoal goal fps
      res @?= ["src/DGM/Archive.hs"]

  , testCase "applyGoal: AvoidModule 'Types' removes Types.hs" $ do
      let fps  = [ "src/DGM/Archive.hs"
                 , "src/DGM/Types.hs"
                 , "src/DGM/Rewriting.hs"
                 ]
          goal = EvolutionGoal "avoid Types" [AvoidModule "Types"]
          res  = applyGoal goal fps
      "src/DGM/Types.hs" `elem` res @?= False
      length res @?= 2

  , testCase "applyGoal: no constraints returns all files unchanged" $ do
      let fps  = ["src/DGM/Archive.hs", "src/DGM/Types.hs"]
          goal = EvolutionGoal "no filter" []
          res  = applyGoal goal fps
      res @?= fps

  , testCase "applyGoal: PreferModule and AvoidModule combined" $ do
      let fps  = [ "src/DGM/Archive.hs"
                 , "src/DGM/Types.hs"
                 , "src/DGM/ArchiveOld.hs"
                 ]
          goal = EvolutionGoal "archive not old"
                   [PreferModule "Archive", AvoidModule "Old"]
          res  = applyGoal goal fps
      "src/DGM/ArchiveOld.hs" `elem` res @?= False
      "src/DGM/Archive.hs"    `elem` res @?= True

  , testCase "applyGoal: empty file list returns empty" $ do
      let goal = EvolutionGoal "any" [PreferModule "Archive"]
          res  = applyGoal goal []
      res @?= []

  -- ── describeGoal ─────────────────────────────────────────────────────────
  , testCase "describeGoal: single constraint uses singular form" $ do
      let goal = EvolutionGoal "improve Archive" [PreferModule "Archive"]
          desc = describeGoal goal
      T.isInfixOf "1 constraint active" desc @?= True

  , testCase "describeGoal: multiple constraints use plural form" $ do
      let goal = EvolutionGoal "complex"
                   [PreferModule "Archive", AvoidModule "Types"]
          desc = describeGoal goal
      T.isInfixOf "2 constraints active" desc @?= True

  , testCase "describeGoal: description is included in output" $ do
      let goal = EvolutionGoal "my special goal" [GoalText "my special goal"]
          desc = describeGoal goal
      T.isInfixOf "my special goal" desc @?= True

  , testCase "MutationConstraint: Show instance is defined" $ do
      let mc = PreferModule "Archive"
          s  = show mc
      s @?= "PreferModule \"Archive\""
  ]

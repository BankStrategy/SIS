-- | Main entry point for the Darwin Gödel Machine MVP.
--
-- Flags:
--   @--self-mod N@  — run N SelfModPhase cycles (Haskell source self-modification).
--                     Exits with code 42 after the first successful mutation commit
--                     (signalling "I improved — restart me" to the supervisor loop).
--   @--evolve N@    — run up to N autonomous generations, looping internally until
--                     no improvement is found or the generation limit is reached.
--   @--goal TEXT@   — set a natural-language evolution goal; filters candidate
--                     source files via 'DGM.NatLang.applyGoal' before each
--                     SelfMod mutation round.  Printed in the banner when set.
--   @N@             — run N ExprPhase (Zero Data) cycles (default: 5)
--
-- The GENERATION environment variable (set by the supervisor or manually) is
-- printed on startup to identify the current evolutionary generation.
module Main where

import Control.Concurrent.STM (atomically, writeTVar, readTVar, modifyTVar')
import Control.Exception (bracket)
import Control.Monad (replicateM, when, forM_)
import Data.IORef (newIORef)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitWith, ExitCode(..))
import Text.Read (readMaybe)

import DGM

-- | Execution mode selected by command-line flags.
data RunMode
  = RunZeroData Int   -- ^ N ExprPhase cycles (default).
  | RunSelfMod  Int   -- ^ N SelfMod cycles; exits 42 after a successful commit.
  | RunEvolve   Int   -- ^ Up to N generation loop (internal restart).

main :: IO ()
main = do
  args <- getArgs
  let mode     = parseArgs args
      mGoalTxt = parseGoalFlag args

  -- Read generation counter from environment (set by dgm-evolve.sh supervisor).
  genNum <- readGeneration

  putStrLn banner
  putStrLn "Oracle: google/gemini-2.5-flash-lite | LH: enabled | SBV: z3-4.15.5"
  when (genNum > 0) $
    putStrLn ("Generation: " <> show genNum <> "\n")

  -- Parse natural-language goal (if supplied via --goal).
  mGoal <- case mGoalTxt of
    Nothing      -> return Nothing
    Just goalTxt -> do
      mOracleEnv <- newOracleEnv
      goal <- case mOracleEnv of
        Just env -> parseGoal env goalTxt
        Nothing  -> return (EvolutionGoal goalTxt [GoalText goalTxt])
      putStrLn ("  " <> T.unpack (describeGoal goal) <> "\n")
      return (Just goal)

  case mode of
    RunZeroData cycles ->
      putStrLn $ "Running " <> show cycles <> " Zero Data Cycle(s) [ExprPhase]...\n"
    RunSelfMod cycles ->
      putStrLn $ "Running " <> show cycles <> " SelfMod Cycle(s) [SelfModPhase]...\n"
    RunEvolve maxGens ->
      putStrLn $ "Evolving up to " <> show maxGens <> " generation(s)...\n"

  -- Initialise agent state.
  -- Capture the repo root (cwd at startup) so SystemWrite can enforce path whitelisting.
  repoRoot <- getCurrentDirectory
  let bootstrapNode = exprToASTNode "root" bootstrapExpr
  st <- newAgentStateWithRoot bootstrapNode repoRoot
  atomically $ writeTVar (stateGeneration st) genNum

  -- Initialise audit log.
  auditLog <- newIORef []

  -- Open archive backend (SQLite by default; survives restarts).
  let backend = SQLiteBacked "dgm-archive.db"
  (stats, committed, dynCount) <- bracket (openArchive backend) closeArchive $ \hdl -> do
    -- Load any previously persisted entries into the TVar.
    prior <- loadArchive hdl
    atomically $ writeTVar (stateArchive st) prior
    when (not (null prior)) $
      putStrLn $ "Loaded " <> show (length prior) <> " entries from archive.\n"

    priorBlacklist <- loadBlacklist hdl
    atomically $ writeTVar (stateMutationBlacklist st)
      (Set.fromList priorBlacklist)
    when (not (null priorBlacklist)) $
      putStrLn $ "Loaded " <> show (length priorBlacklist) <> " blacklisted mutations.\n"

    -- Load persisted dynamic rules and evaluate them via HintBridge.
    priorRules <- loadDynamicRules hdl
    when (not (null priorRules)) $
      putStrLn $ "Loading " <> show (length priorRules) <> " persisted dynamic rules...\n"

    -- Build cycle configuration; inject goal if provided.
    cfg0 <- defaultCycleConfig st auditLog hdl
    let cfg = cfg0 { ccEvolutionGoal = mGoal }

    -- Evaluate persisted rule snippets and inject into live rule set.
    forM_ priorRules $ \(desc, snippet, sc) -> do
      eResult <- evalRuleCandidate (ccHintEnv cfg) snippet
      case eResult of
        Left _err -> pure ()   -- Skip rules that fail to evaluate.
        Right fn  -> atomically $ modifyTVar' (ccDynamicRules cfg)
          (DynamicRule desc fn sc :)

    case mode of
      RunZeroData cycles -> do
        s    <- runCycleN cfg cycles
        dyn  <- atomically (readTVar (ccDynamicRules cfg))
        return (s, False, length dyn)

      RunSelfMod cycles -> do
        -- Run N SelfMod cycles; track if any committed successfully.
        stepsList <- replicateM cycles (runCycle cfg SelfModPhase)
        entries   <- atomically (getAll (stateArchive st))
        dyn       <- atomically (readTVar (ccDynamicRules cfg))
        let didCommit = any hasSuccessfulCommit stepsList
        return (computeStats entries, didCommit, length dyn)

      RunEvolve maxGens -> do
        s    <- runEvolveN cfg genNum maxGens
        dyn  <- atomically (readTVar (ccDynamicRules cfg))
        return (s, False, length dyn)

  -- Print summary.
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Archive Statistics"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn $ "Total entries:         " <> show (asTotal stats)
  putStrLn $ "Passed:                " <> show (asPassed stats)
  putStrLn $ "Failed (stepping stones): " <> show (asFailed stats)
  putStrLn $ "Generations:           " <> show (asGenerations stats)
  putStrLn $ "Dynamic rules loaded:  " <> show dynCount
  if asTotal stats > 0
    then do
      putStrLn $ "Best score:            " <> show (asBestScore stats)
      putStrLn $ "Mean score:            " <> show (round2 (asMeanScore stats))
    else
      putStrLn "(no entries yet)"

  putStrLn "\nDone. The capability ledger has been updated."

  -- Exit 42 signals "I improved — restart me" to the dgm-evolve.sh supervisor.
  when committed $
    exitWith (ExitFailure 42)

-- | Run up to @maxGens@ SelfMod cycles, stopping when no improvement is found.
--
-- On each cycle that commits successfully, the generation counter is
-- incremented and the loop continues.  On the first cycle with no improvement,
-- the loop exits early and returns archive statistics.
runEvolveN :: CycleConfig -> Int -> Int -> IO ArchiveStats
runEvolveN cfg startGen maxGens = go startGen maxGens
  where
    go _   0         = readStats
    go gen remaining = do
      steps <- runCycle cfg SelfModPhase
      if hasSuccessfulCommit steps
        then do
          let gen' = gen + 1
          putStrLn ("[dgm] Generation " <> show gen' <> " evolved successfully.")
          go gen' (remaining - 1)
        else do
          putStrLn "[dgm] No improvement found; evolution complete."
          readStats

    readStats = do
      entries <- atomically (getAll (stateArchive (ccAgentState cfg)))
      return (computeStats entries)

-- | Read the GENERATION environment variable (default 0).
readGeneration :: IO Int
readGeneration = do
  mval <- lookupEnv "GENERATION"
  return $ case mval >>= readMaybe of
    Nothing -> 0
    Just n  -> n

-- | Parse command-line arguments into a 'RunMode'.
--
-- Recognised forms:
--   @--self-mod N@  → RunSelfMod N
--   @--evolve N@    → RunEvolve N
--   @--goal TEXT@   → (consumed by 'parseGoalFlag'; skipped here)
--   @N@             → RunZeroData N
--   @(nothing)@     → RunZeroData 5
parseArgs :: [String] -> RunMode
parseArgs ("--self-mod" : n : _)     = RunSelfMod (maybe 1 id (readMaybe n))
parseArgs ("--self-mod" : _)         = RunSelfMod 1
parseArgs ("--evolve"   : n : _)     = RunEvolve  (maybe 1 id (readMaybe n))
parseArgs ("--evolve"   : _)         = RunEvolve  1
parseArgs ("--goal"     : _ : rest)  = parseArgs rest   -- skip goal text, continue
parseArgs ("--goal"     : _)         = RunZeroData 5
parseArgs (n : _)                    = RunZeroData (maybe 5 id (readMaybe n))
parseArgs []                         = RunZeroData 5

-- | Extract the @--goal TEXT@ argument from the command line, if present.
parseGoalFlag :: [String] -> Maybe Text
parseGoalFlag ("--goal" : txt : _) = Just (T.pack txt)
parseGoalFlag (_ : rest)           = parseGoalFlag rest
parseGoalFlag []                   = Nothing

round2 :: Double -> Double
round2 x = fromIntegral (round (x * 100) :: Int) / 100.0

banner :: String
banner = unlines
  [ "┌─────────────────────────────────────────────────────────┐"
  , "│  Darwin Gödel Machine — Haskell MVP                    │"
  , "│  Synthesising DGM + Absolute Zero in Haskell           │"
  , "│                                                         │"
  , "│  Architecture:                                          │"
  , "│    • GADT-based type-driven safety (§3)                 │"
  , "│    • Hylomorphic evolutionary loop (§4)                 │"
  , "│    • STM transactional rollback (§3.2)                  │"
  , "│    • Formal verification gate (§2.3)                    │"
  , "│    • Absolute Zero self-play (§1.2)                     │"
  , "│    • DGM expanding archive (§1.1)                       │"
  , "└─────────────────────────────────────────────────────────┘"
  ]

-- | DGM.SelfMod — Self-modification orchestration layer (SELFMOD.md Phase C2).
--
-- Reads the agent's own @.hs@ source files, generates 'HsMutation' candidates
-- via 'DGM.HsAST', ranks them, and writes selected mutations back through the
-- 'DGM.SafetyKernel' existential gate.
--
-- Safety invariants (SELFMOD.md Phase C):
--
-- 1. __Path whitelist__: only @src\/DGM\/@-rooted paths are accepted.
--    'DGM.SafetyKernel.dispatchExistential' enforces this for any @src\/…@
--    path; 'writeMutation' additionally checks that the @\/DGM\/@ component is
--    present so we never accidentally overwrite non-DGM sources.
--
-- 2. __Atomic write + parse verification__: handled by
--    'DGM.SafetyKernel.atomicWriteFile' (temp-file + @rename@).
--
-- 3. __Never write the currently-executing module__: callers should filter
--    loaded modules out of the 'discoverSources' result before calling
--    'proposeSelfMutations'.
module DGM.SelfMod
  ( -- * Source discovery
    discoverSources
    -- * Parsing
  , readOwnSource
    -- * Mutation pipeline
  , proposeSelfMutations
  , rankMutations
    -- * Writing
  , writeMutation
  , writeMutationTyped
    -- * Committing
  , commitMutation
  , commitMessage
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Exception (catch, IOException)
import Control.Monad (unless)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist, canonicalizePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension, splitDirectories, normalise, takeDirectory, takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

import DGM.Types
import DGM.HsAST
import DGM.SafetyKernel
import DGM.ModGraph (ModuleGraph, moduleImpact)
import DGM.Oracle (newOracleEnv, MutationContext)
import DGM.OracleHandle (withOracle, proposeMutationH)
import DGM.Reversal (Invertible(..), TypedTxn(..))

-- ─────────────────────────────────────────────────────────────────────────────
-- Source discovery
-- ─────────────────────────────────────────────────────────────────────────────

-- | List all @.hs@ files under @src\/DGM\/@, returning full (relative) paths.
--
-- Paths are of the form @\"src\/DGM\/Foo.hs\"@, suitable for passing to
-- 'readOwnSource' or as the @FilePath@ in a 'SystemWrite' command.
discoverSources :: IO [FilePath]
discoverSources = do
  let dir = "src" </> "DGM"
  files <- listDirectory dir
  let hsFiles = filter (\f -> takeExtension f == ".hs") files
  return (map (dir </>) hsFiles)

-- ─────────────────────────────────────────────────────────────────────────────
-- Parsing
-- ─────────────────────────────────────────────────────────────────────────────

-- | Parse a single Haskell source file via 'parseHsFile'.
--
-- On parse error the error message is logged to @stderr@ (for diagnostic
-- visibility) and returned as @'Left'@.
readOwnSource :: FilePath -> IO (Either Text HsModule)
readOwnSource fp = do
  result <- parseHsFile fp
  case result of
    Left err -> do
      hPutStrLn stderr ("DGM.SelfMod: parse error in " ++ fp ++ ": " ++ T.unpack err)
      return (Left err)
    Right m -> return (Right m)

-- ─────────────────────────────────────────────────────────────────────────────
-- Mutation pipeline
-- ─────────────────────────────────────────────────────────────────────────────

-- | Collect all applicable mutations across a set of source files.
--
-- For each file that parses successfully, queries the oracle (if available)
-- for a proposed mutation and then appends heuristic candidates from
-- 'collectHsMutations' \/ 'defaultHsRules'.  Oracle mutations are prepended
-- so that the ranking step sees them first.
--
-- If no oracle key is present (@OPENROUTER_API_KEY@ or @.env@), the oracle
-- call is skipped and only heuristic mutations are returned — the system
-- remains fully functional without network access.
--
-- The @originalModule@ in each triple is the /unparsed/ form; apply
-- 'applyHsMutation' to obtain the mutated 'HsModule' before calling
-- 'writeMutation'.
proposeSelfMutations
  :: AgentState
  -> [FilePath]
  -> Maybe MutationContext  -- ^ Archive context to guide the oracle.
  -> IO [(FilePath, HsMutation, HsModule)]
proposeSelfMutations st fps mCtx = do
  blacklist  <- atomically $ readTVar (stateMutationBlacklist st)
  mOracleEnv <- newOracleEnv
  perFile    <- mapM (\fp -> do
    result <- readOwnSource fp
    case result of
      Left _  -> return []
      Right m -> do
        let notBlacklisted (f, mut, _) =
              (f, hmDescription mut) `Set.notMember` blacklist
            heuristicMuts = filter notBlacklisted
              [(fp, mut, m) | mut <- collectHsMutations defaultHsRules m]
        oracleMuts <- case mOracleEnv of
          Nothing  -> return []
          Just env -> do
            src <- TIO.readFile fp
            let tests = map hnText (filter (\n -> hnKind n == "value") (hsNodes m))
            eOrMut <- withOracle env $ \h -> proposeMutationH h fp src tests mCtx
            case eOrMut of
              Left err -> do
                unless ("build with -f+with-oracle" `T.isInfixOf` err) $
                  hPutStrLn stderr ("DGM.SelfMod: oracle error for " ++ fp ++ ": " ++ T.unpack err)
                return []
              Right om ->
                return (filter notBlacklisted [(fp, om, m)])
        return (oracleMuts ++ heuristicMuts)) fps
  return (concat perFile)

-- | Sort mutation candidates for selection.
--
-- Ordering criterion (ascending tuple = "best first"):
--
--   0. __Module impact__ (Phase F) — number of modules whose exported
--      interface is affected by this mutation.  Mutations that do not change
--      any exported name score 0 here and rank first.  Mutations touching a
--      heavily-depended-on module (e.g. @Types.hs@) receive a large penalty
--      equal to @'moduleImpact' mg fp mut m@.
--
--   1. __Mutation size__ — absolute difference in text length between the
--      original and mutated module (smaller diff = less disruptive = ranked
--      first).  When the mutation is inapplicable (should not occur after
--      'proposeSelfMutations'), the original length is used as a no-diff
--      placeholder.
--
--   2. __Novelty__ — mutations whose 'hmDescription' appears earlier in the
--      unique-description ordering of the candidate list are considered more
--      novel (i.e. they represent rule types not yet seen in this batch).
--      Within a description group, the file path is used as a tiebreak.
--
-- Returns the list sorted with the most desirable candidate first.
rankMutations
  :: ModuleGraph
  -> [(FilePath, HsMutation, HsModule)]
  -> [(FilePath, HsMutation, HsModule)]
rankMutations _  [] = []
rankMutations mg triples = sortBy (comparing mkScore) triples
  where
    -- Ordered list of first-seen descriptions (most novel = smallest index).
    seenOrder :: [Text]
    seenOrder = nubPreserve (map (\(_, mut, _) -> hmDescription mut) triples)

    nubPreserve :: [Text] -> [Text]
    nubPreserve = foldr (\x acc -> if x `elem` acc then acc else acc ++ [x]) []

    mkScore :: (FilePath, HsMutation, HsModule) -> (Int, Int, Int, FilePath)
    mkScore (fp, mut, m) = (impact, sizeDiff, descIdx, fp)
      where
        impact   = moduleImpact mg fp mut m

        origLen  = T.length (printHsModule m)
        mutLen   = case applyHsMutation mut m of
                     Left _   -> origLen   -- inapplicable; treat as zero diff
                     Right m' -> T.length (printHsModule m')
        sizeDiff = abs (mutLen - origLen)

        descIdx  = length (takeWhile (/= hmDescription mut) seenOrder)

-- ─────────────────────────────────────────────────────────────────────────────
-- Writing
-- ─────────────────────────────────────────────────────────────────────────────

-- | Write a (already-mutated) 'HsModule' to @fp@ via the SafetyKernel.
--
-- Two layers of path whitelisting are applied:
--
--   1. __Local check__ (this function): @fp@ must contain a @\"DGM\"@
--      directory component.  This is tighter than the SafetyKernel's
--      @src\/@ whitelist and ensures @SelfMod@ can only write DGM sources.
--
--   2. __SafetyKernel check__ ('dispatchExistential'): verifies that @fp@ is
--      under the 'stateRepoRoot' @\/src\/@ directory and that the
--      'QuorumProof' covers exactly this operation.
--
-- The @m@ argument is the __mutated__ module (e.g. the result of
-- @'applyHsMutation' mut original@); its text is obtained via 'printHsModule'.
writeMutation
  :: AgentState
  -> AuditLog
  -> QuorumProof
  -> FilePath      -- ^ Target path (must be under @src\/DGM\/@).
  -> HsModule      -- ^ The mutated module to write.
  -> IO (Either Text ())
writeMutation st audit proof fp m = do
  -- Layer 1: local DGM path check.
  if not (isDGMPath fp)
    then return (Left ("writeMutation: path must be under src/DGM/: " <> T.pack fp))
    else do
      let content = printHsModule m
          cmd     = SystemWrite fp content proof
      -- Layer 2: SafetyKernel existential gate.
      result <- dispatchExistential st audit cmd
      case result of
        Left err -> return (Left err)
        Right _  -> return (Right ())

-- | Like 'writeMutation' but also returns a 'TypedTxn' for rollback.
--
-- Reads the current file content before writing, then — on success —
-- constructs a 'TypedTxn' whose 'DGM.Reversal.backward' function restores
-- the original content.  Callers can pass the returned 'TypedTxn' to
-- 'DGM.Reversal.rollbackTypedTxn' if subsequent tests fail, replacing the
-- snapshot-and-restore approach of 'DGM.SelfCompile.withSelfModTxn'.
--
-- The forward / backward functions are whole-file replacements: @forward@
-- re-applies the mutated text, @backward@ restores the original.  This
-- satisfies the invertibility law:
--
-- @
--   backward (forward x) == x     — for x == original content
-- @
writeMutationTyped
  :: AgentState
  -> AuditLog
  -> QuorumProof
  -> FilePath      -- ^ Target path (must be under @src\/DGM\/@).
  -> HsModule      -- ^ The mutated module to write.
  -> HsMutation    -- ^ The originating mutation (stored in the transaction).
  -> IO (Either Text TypedTxn)
writeMutationTyped st audit proof fp m mut = do
  -- Snapshot original content for the backward function.
  originalText <- TIO.readFile fp
    `catch` (\(_ :: IOException) -> return "")
  let mutatedText = printHsModule m
      inv = Invertible
        { forward        = const mutatedText
        , backward       = const originalText
        , invDescription = "whole-file write in " <> T.pack fp
        }
  result <- writeMutation st audit proof fp m
  case result of
    Left err -> return (Left err)
    Right () ->
      return (Right TypedTxn
        { txnFile     = fp
        , txnOp       = inv
        , txnMutation = mut
        })

-- | True when @fp@ contains a @\"DGM\"@ path component.
--
-- Handles both relative paths (@\"src\/DGM\/Foo.hs\"@) and absolute paths
-- (@\"\/abs\/path\/src\/DGM\/Foo.hs\"@).
isDGMPath :: FilePath -> Bool
isDGMPath fp = "DGM" `elem` splitDirectories (normalise fp)

-- ─────────────────────────────────────────────────────────────────────────────
-- Committing
-- ─────────────────────────────────────────────────────────────────────────────

-- | Commit a successfully-tested mutation to git.
--
-- Resolves @path@ to an absolute path, then walks up the directory tree to
-- locate the git repository root.  All git sub-commands are run with
-- @git -C \<root\>@ so the function is safe regardless of the calling
-- process's current working directory.
--
-- __Idempotency__: checks @git status --porcelain \<path\>@ first.  If the
-- file is already clean (it was previously committed, e.g. after a
-- crash-and-restart), the existing HEAD hash is returned without creating a
-- duplicate commit.
--
-- Returns @Right gitHash@ on success, @Left err@ on any git failure.
commitMutation
  :: FilePath      -- ^ Path to the mutated file (relative or absolute).
  -> HsMutation    -- ^ The winning mutation (used to build the commit message).
  -> ArchiveEntry  -- ^ Archive entry associated with this commit.
  -> IO (Either Text Text)
commitMutation path mutation entry = do
  absPath <- canonicalizePath path
  mRoot   <- findGitRoot (takeDirectory absPath)
  case mRoot of
    Nothing ->
      return (Left ("commitMutation: not in a git repository: " <> T.pack absPath))
    Just gitRoot -> do
      -- Idempotency check: is the file already committed (clean working tree)?
      (sc, statusOut, statusErr) <-
        readProcessWithExitCode "git"
          ["-C", gitRoot, "status", "--porcelain", absPath] ""
      case sc of
        ExitFailure _ ->
          return (Left ("git status failed: " <> T.pack statusErr))
        ExitSuccess ->
          if T.null (T.strip (T.pack statusOut))
            then
              -- Working tree is clean — mutation was already committed.
              gitRevParseHead gitRoot
            else do
              -- Stage the mutated file.
              (ac, _, addErr) <-
                readProcessWithExitCode "git" ["-C", gitRoot, "add", absPath] ""
              case ac of
                ExitFailure _ ->
                  return (Left ("git add failed: " <> T.pack addErr))
                ExitSuccess -> do
                  let msg = commitMessage path mutation entry
                  (cc, _, commitErr) <-
                    readProcessWithExitCode "git"
                      ["-C", gitRoot, "commit", "-m", T.unpack msg] ""
                  case cc of
                    ExitFailure _ -> do
                      -- Unstage so the working tree stays in modified (unstaged) state.
                      _ <- readProcessWithExitCode "git"
                             ["-C", gitRoot, "reset", "HEAD", absPath] ""
                      return (Left ("git commit failed: " <> T.pack commitErr))
                    ExitSuccess ->
                      gitRevParseHead gitRoot

-- | Build the git commit message for a self-modification.
--
-- Format: @selfmod: \<description\> in \<module\> (gen \<n\>) [entry:\<id\>]@
--
-- Truncated to 72 characters if the full string exceeds that limit.  Full
-- mutation details live in the archive entry, not the commit message.
commitMessage :: FilePath -> HsMutation -> ArchiveEntry -> Text
commitMessage path mutation entry =
  let modName = T.pack (takeBaseName path)
      gen     = entryGeneration entry
      eid     = entryId entry
      desc    = hmDescription mutation
      full    = "selfmod: " <> desc
             <> " in " <> modName
             <> " (gen " <> T.pack (show gen) <> ")"
             <> " [entry:" <> eid <> "]"
  in  if T.length full > 72 then T.take 69 full <> "..." else full

-- | Walk up the directory tree from @dir@, looking for a @.git@ entry.
--
-- Handles both regular git repositories (@.git\/@ directory) and git worktrees
-- (@.git@ file pointing to the real @.git@ directory).
--
-- Returns @Just root@ when found, @Nothing@ if the filesystem root is
-- reached without finding a repository.
findGitRoot :: FilePath -> IO (Maybe FilePath)
findGitRoot dir = do
  let dotGit = dir </> ".git"
  isDir  <- doesDirectoryExist dotGit
  isFile <- doesFileExist dotGit
  if isDir || isFile
    then return (Just dir)
    else do
      let parent = takeDirectory dir
      if parent == dir   -- reached the filesystem root
        then return Nothing
        else findGitRoot parent

-- | Run @git rev-parse HEAD@ in @gitRoot@ and return the trimmed commit hash.
gitRevParseHead :: FilePath -> IO (Either Text Text)
gitRevParseHead gitRoot = do
  (ec, hashOut, hashErr) <-
    readProcessWithExitCode "git" ["-C", gitRoot, "rev-parse", "HEAD"] ""
  case ec of
    ExitSuccess   -> return (Right (T.strip (T.pack hashOut)))
    ExitFailure _ -> return (Left ("git rev-parse HEAD failed: " <> T.pack hashErr))

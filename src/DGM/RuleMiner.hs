-- | DGM.RuleMiner — Mine successful mutations from the archive to improve
-- future mutation selection.
--
-- The archive preserves both successful and failed mutations.  This module
-- analyses that history to extract 'MutationPattern's — statistical summaries
-- of how well different categories of mutations have performed.  These
-- patterns are then used to bias candidate ranking ('boostByFile') and to
-- prune low-value dynamic rules ('evictStaleRules').
module DGM.RuleMiner
  ( -- * Pattern type
    MutationPattern(..)
    -- * Mining
  , minePatterns
    -- * Querying
  , successRate
    -- * Ranking integration
  , boostByFile
    -- * Rule eviction
  , evictStaleRules
  ) where

import Data.List (sortBy, groupBy, nub)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T

import DGM.Types (ArchiveEntry(..), Mutation(..))
import DGM.Rewriting (DynamicRule(..))

-- ---------------------------------------------------------------------------
-- Pattern type
-- ---------------------------------------------------------------------------

-- | A statistical summary of mutations sharing a common description prefix.
--
-- Patterns group archive entries by the first word of 'entryCode' (or
-- @\"oracle-diff\"@ for oracle-proposed mutations) and track how many
-- succeeded, how many failed, the best score achieved, and which target
-- files saw successes.
data MutationPattern = MutationPattern
  { mpRulePrefix  :: Text      -- ^ Mutation description prefix (first word).
  , mpSuccesses   :: Int       -- ^ Number of passing entries in this group.
  , mpFailures    :: Int       -- ^ Number of failing entries in this group.
  , mpBestScore   :: Double    -- ^ Highest score achieved by this group.
  , mpTargetFiles :: [Text]    -- ^ Deduplicated files where this rule succeeded.
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Mining
-- ---------------------------------------------------------------------------

-- | Extract 'MutationPattern's from a list of archive entries.
--
-- Entries are grouped by a normalised prefix derived from 'entryCode':
--
--   * If the code starts with @\"oracle-diff\"@ the prefix is @\"oracle-diff\"@.
--   * Otherwise the prefix is the first whitespace-delimited word.
--   * Empty descriptions are bucketed under @\"unknown\"@.
--
-- Within each group the function computes success\/failure counts, the best
-- score, and the deduplicated list of target files that saw a success.
minePatterns :: [ArchiveEntry] -> [MutationPattern]
minePatterns entries =
  map buildPattern grouped
  where
    -- Tag each entry with its normalised prefix.
    tagged :: [(Text, ArchiveEntry)]
    tagged = map (\e -> (normalisePrefix (entryCode e), e)) entries

    -- Sort then group by prefix so 'groupBy' produces contiguous buckets.
    sorted :: [(Text, ArchiveEntry)]
    sorted = sortBy (\a b -> compare (fst a) (fst b)) tagged

    grouped :: [[(Text, ArchiveEntry)]]
    grouped = groupBy (\a b -> fst a == fst b) sorted

    buildPattern :: [(Text, ArchiveEntry)] -> MutationPattern
    buildPattern [] = MutationPattern "" 0 0 0.0 []  -- unreachable
    buildPattern grp@((prefix, _) : _) =
      let es        = map snd grp
          successes = filter entryPassed es
          failures  = filter (not . entryPassed) es
          best      = if null es then 0.0 else maximum (map entryScore es)
          -- Collect target files from the mutation field of successful entries.
          files     = nub $ concatMap targetFile successes
      in MutationPattern
           { mpRulePrefix  = prefix
           , mpSuccesses   = length successes
           , mpFailures    = length failures
           , mpBestScore   = best
           , mpTargetFiles = files
           }

    targetFile :: ArchiveEntry -> [Text]
    targetFile e = case entryMutation e of
      Just mut -> [mutationTarget mut]
      Nothing  -> []

-- | Normalise an entry's code text into a grouping prefix.
--
-- Oracle-proposed mutations are identified by the @\"oracle-diff\"@ prefix
-- that 'DGM.Oracle' prepends to their descriptions.
normalisePrefix :: Text -> Text
normalisePrefix code
  | T.isPrefixOf "oracle-diff" code = "oracle-diff"
  | otherwise =
      case T.words code of
        (w : _) -> T.toLower w
        []      -> "unknown"

-- ---------------------------------------------------------------------------
-- Querying
-- ---------------------------------------------------------------------------

-- | Compute the success rate for a pattern: @successes / (successes + failures)@.
--
-- Returns @0.0@ when there is no data (zero total attempts).
successRate :: MutationPattern -> Double
successRate mp
  | total == 0 = 0.0
  | otherwise  = fromIntegral (mpSuccesses mp) / fromIntegral total
  where
    total = mpSuccesses mp + mpFailures mp

-- ---------------------------------------------------------------------------
-- Ranking integration
-- ---------------------------------------------------------------------------

-- | Re-order mutation candidates by preferring files where mutations have
-- historically succeeded.
--
-- Each candidate's file path is looked up against the mined patterns.  If
-- the file appears in any pattern's 'mpTargetFiles', the candidate receives
-- that pattern's 'successRate' as a boost score.  When a file matches
-- multiple patterns, the maximum success rate is taken.
--
-- Candidates are sorted by descending boost score; ties preserve original
-- relative order (stable sort).
boostByFile :: [MutationPattern] -> [(FilePath, a, b)] -> [(FilePath, a, b)]
boostByFile [] candidates = candidates
boostByFile patterns candidates =
  map snd $ sortBy cmpBoost indexed
  where
    -- Attach original index to preserve stable ordering on ties.
    indexed = zip [(0 :: Int)..] candidates

    -- Build a quick lookup: for each file that ever succeeded, what is the
    -- best success rate across all patterns that mention it?
    fileBoost fp =
      let fpText     = T.pack fp
          matching   = filter (\p -> fpText `elem` mpTargetFiles p) patterns
          rates      = map successRate matching
      in case rates of
           [] -> 0.0
           _  -> maximum rates

    -- Primary: higher boost first.  Secondary: original index (stable).
    cmpBoost (i1, (fp1, _, _)) (i2, (fp2, _, _)) =
      case compare (Down (fileBoost fp1)) (Down (fileBoost fp2)) of
        EQ -> compare i1 i2
        x  -> x

-- ---------------------------------------------------------------------------
-- Rule eviction
-- ---------------------------------------------------------------------------

-- | Evict low-scoring dynamic rules when the pool exceeds a size limit.
--
-- If the rule list is at or below the limit, it is returned unchanged.
-- Otherwise, rules with @drScore <= 0@ are removed first.  If the list is
-- still over the limit, the top-N rules by descending score are kept.
evictStaleRules :: Int -> [DynamicRule] -> [DynamicRule]
evictStaleRules limit rules
  | length rules <= limit = rules
  | otherwise =
      let pruned = filter (\r -> drScore r > 0) rules
      in take limit $ sortBy (\a b -> compare (Down (drScore a)) (Down (drScore b))) pruned

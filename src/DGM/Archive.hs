{-# LANGUAGE CPP #-}
-- | DGM.Archive — The Darwin Gödel Machine expanding archive (SPEC.md §1.1).
--
-- The archive is the evolutionary memory of the system.  Crucially, *failed*
-- mutations are also preserved alongside successful ones.  The optimisation
-- landscape of self-improvement is non-convex: a dead-end today may be a
-- necessary stepping-stone to a future breakthrough.
--
-- Implementation: the archive is backed by a 'TVar' list (O(1) prepend) and
-- supports probabilistic sampling weighted by both score and recency.
module DGM.Archive
  ( -- * Core operations
    addEntry
  , getAll
  , getBest
  , sampleWeighted
  , getLineage
    -- * Statistics
  , ArchiveStats(..)
  , computeStats
    -- * Persistence handle
  , ArchiveHandle
  , openArchive
  , closeArchive
  , flushArchive
  , loadArchive
  , flushBlacklist
  , loadBlacklist
  , flushDynamicRules
  , loadDynamicRules
    -- * Serialisation
  , archiveToJSON
  , jsonToArchive
  ) where

import Control.Concurrent.STM
import Data.List (sortBy, maximumBy, minimumBy)
import Data.Ord (comparing, Down(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, mapMaybe)
import System.Random (randomRIO)
import Data.Aeson (encode, decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import DGM.Types

#ifdef WITH_SQLITE
import qualified Database.SQLite.Simple as SQL
#endif

-- ─────────────────────────────────────────────────────────────────────────────
-- Core operations
-- ─────────────────────────────────────────────────────────────────────────────

-- | Append an entry to the front of the archive (O(1)).
addEntry :: TVar [ArchiveEntry] -> ArchiveEntry -> STM ()
addEntry archVar entry = modifyTVar' archVar (entry :)

-- | Read a snapshot of the full archive.
getAll :: TVar [ArchiveEntry] -> STM [ArchiveEntry]
getAll = readTVar

-- | Return the entry with the highest score.
{-@ getBest :: TVar {xs:[ArchiveEntry] | True} -> STM (Maybe ArchiveEntry) @-}
getBest :: TVar [ArchiveEntry] -> STM (Maybe ArchiveEntry)
getBest archVar = do
  entries <- readTVar archVar
  pure $ case entries of
    [] -> Nothing
    _  -> Just (maximumBy (comparing entryScore) entries)

-- | Weighted sampling from the archive.
--
-- Weight is proportional to @exp(score)@, biasing towards higher-scoring
-- entries while still sampling the full distribution (necessary for
-- escaping local optima, per SPEC.md §1.1).
sampleWeighted :: TVar [ArchiveEntry] -> IO (Maybe ArchiveEntry)
sampleWeighted archVar = do
  entries <- atomically (readTVar archVar)
  case entries of
    [] -> pure Nothing
    _  -> do
      let weights = map (exp . entryScore) entries
          total   = sum weights
          normed  = map (/ total) weights
          cumul   = scanl1 (+) normed
      r <- randomRIO (0.0, 1.0)
      let idx = length (takeWhile (< r) cumul)
          sel = entries !! min idx (length entries - 1)
      pure (Just sel)

-- | Retrieve the full lineage (ancestor chain) for an entry.
getLineage :: TVar [ArchiveEntry] -> Text -> STM [ArchiveEntry]
getLineage archVar entryId0 = do
  entries <- readTVar archVar
  let byId = Map.fromList [(entryId e, e) | e <- entries]
  pure (buildChain byId entryId0 [])
  where
    buildChain byId eid acc =
      case Map.lookup eid byId of
        Nothing -> acc
        Just e  ->
          let acc' = e : acc
          in  case entryParentId e of
                Nothing  -> acc'
                Just pid -> buildChain byId pid acc'

-- ─────────────────────────────────────────────────────────────────────────────
-- Statistics
-- ─────────────────────────────────────────────────────────────────────────────

data ArchiveStats = ArchiveStats
  { asTotal         :: Int
  , asPassed        :: Int
  , asFailed        :: Int
  , asBestScore     :: Double
  , asWorstScore    :: Double
  , asMeanScore     :: Double
  , asGenerations   :: Int
  , asMutationTypes :: Map Text Int
  } deriving (Show)

{-@ computeStats :: [ArchiveEntry] -> ArchiveStats @-}
computeStats :: [ArchiveEntry] -> ArchiveStats
computeStats [] = ArchiveStats 0 0 0 0 0 0 0 Map.empty
computeStats entries =
  ArchiveStats
    { asTotal         = total
    , asPassed        = length (filter entryPassed entries)
    , asFailed        = length (filter (not . entryPassed) entries)
    , asBestScore     = entryScore (maximumBy (comparing entryScore) entries)
    , asWorstScore    = entryScore (minimumBy (comparing entryScore) entries)
    , asMeanScore     = sum scores / fromIntegral total
    , asGenerations   = maximum (map entryGeneration entries)
    , asMutationTypes = countMutTypes
    }
  where
    total       = length entries
    scores      = map entryScore entries
    countMutTypes =
      foldr (\e m ->
        case entryMutation e of
          Nothing  -> m
          Just mut ->
            let k = T.pack (show (mutationType mut))
            in  Map.insertWith (+) k 1 m)
        Map.empty entries

-- ─────────────────────────────────────────────────────────────────────────────
-- JSON serialisation (instances live in DGM.Types to avoid -Worphans)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Serialise the full archive to JSON bytes.
archiveToJSON :: [ArchiveEntry] -> BL.ByteString
archiveToJSON = encode

-- | Deserialise archive from JSON bytes.
jsonToArchive :: BL.ByteString -> Maybe [ArchiveEntry]
jsonToArchive = decode

-- ─────────────────────────────────────────────────────────────────────────────
-- Persistence handle
-- ─────────────────────────────────────────────────────────────────────────────

-- | Opaque handle to the archive backend.
--
-- Carries callbacks so that 'Cycle' code never needs to import SQLite directly.
data ArchiveHandle = ArchiveHandle
  { ahFlush          :: [ArchiveEntry] -> IO ()
  , ahLoad           :: IO [ArchiveEntry]
  , ahClose          :: IO ()
  , ahFlushBlacklist :: [(FilePath, Text)] -> IO ()
    -- ^ Persist (filePath, mutationDesc) pairs that have failed.
  , ahLoadBlacklist  :: IO [(FilePath, Text)]
    -- ^ Load all persisted blacklist entries on startup.
  , ahFlushDynamicRules :: [(Text, Text, Double)] -> IO ()
    -- ^ Persist (description, snippet, score) triples for successful dynamic rules.
  , ahLoadDynamicRules  :: IO [(Text, Text, Double)]
    -- ^ Load all persisted dynamic rules on startup.
  }

-- | Open an archive handle for the given backend.
--
-- For 'InMemory' all callbacks are no-ops.  For 'SQLiteBacked' the table
-- is created if it does not already exist and the handle owns the connection.
openArchive :: ArchiveBackend -> IO ArchiveHandle
openArchive InMemory = pure ArchiveHandle
  { ahFlush            = \_ -> pure ()
  , ahLoad             = pure []
  , ahClose            = pure ()
  , ahFlushBlacklist   = \_ -> pure ()
  , ahLoadBlacklist    = pure []
  , ahFlushDynamicRules = \_ -> pure ()
  , ahLoadDynamicRules  = pure []
  }
#ifdef WITH_SQLITE
openArchive (SQLiteBacked path) = do
  conn <- SQL.open path
  SQL.execute_ conn sqlCreateTable
  SQL.execute_ conn sqlCreateBlacklistTable
  SQL.execute_ conn sqlCreateDynamicRulesTable
  pure ArchiveHandle
    { ahFlush            = sqlFlush conn
    , ahLoad             = sqlLoad conn
    , ahClose            = SQL.close conn
    , ahFlushBlacklist   = sqlFlushBlacklist conn
    , ahLoadBlacklist    = sqlLoadBlacklist conn
    , ahFlushDynamicRules = sqlFlushDynamicRules conn
    , ahLoadDynamicRules  = sqlLoadDynamicRules conn
    }
#else
openArchive (SQLiteBacked _) = do
  putStrLn "Warning: SQLite support not compiled in; falling back to in-memory archive"
  pure ArchiveHandle
    { ahFlush            = \_ -> pure ()
    , ahLoad             = pure []
    , ahClose            = pure ()
    , ahFlushBlacklist   = \_ -> pure ()
    , ahLoadBlacklist    = pure []
    , ahFlushDynamicRules = \_ -> pure ()
    , ahLoadDynamicRules  = pure []
    }
#endif

-- | Close the archive handle and release any resources.
closeArchive :: ArchiveHandle -> IO ()
closeArchive = ahClose

-- | Flush a list of entries to persistent storage.
flushArchive :: ArchiveHandle -> [ArchiveEntry] -> IO ()
flushArchive = ahFlush

-- | Load all persisted entries from storage.
loadArchive :: ArchiveHandle -> IO [ArchiveEntry]
loadArchive = ahLoad

-- | Persist a batch of (filePath, mutationDesc) blacklist entries.
flushBlacklist :: ArchiveHandle -> [(FilePath, Text)] -> IO ()
flushBlacklist = ahFlushBlacklist

-- | Load all persisted blacklist entries from storage.
loadBlacklist :: ArchiveHandle -> IO [(FilePath, Text)]
loadBlacklist = ahLoadBlacklist

-- | Persist a batch of (description, snippet, score) dynamic rule triples.
flushDynamicRules :: ArchiveHandle -> [(Text, Text, Double)] -> IO ()
flushDynamicRules = ahFlushDynamicRules

-- | Load all persisted dynamic rules from storage.
loadDynamicRules :: ArchiveHandle -> IO [(Text, Text, Double)]
loadDynamicRules = ahLoadDynamicRules

-- ─────────────────────────────────────────────────────────────────────────────
-- SQLite implementation (compiled only when WITH_SQLITE is defined)
-- ─────────────────────────────────────────────────────────────────────────────

#ifdef WITH_SQLITE

sqlCreateTable :: SQL.Query
sqlCreateTable = "CREATE TABLE IF NOT EXISTS archive_entries (id TEXT PRIMARY KEY, code TEXT NOT NULL, parent_id TEXT, score REAL NOT NULL, passed INTEGER NOT NULL, generation INTEGER NOT NULL, counter_ex TEXT)"

-- | Upsert a batch of entries inside a single transaction.
sqlFlush :: SQL.Connection -> [ArchiveEntry] -> IO ()
sqlFlush conn entries = SQL.withTransaction conn $
  mapM_ (sqlUpsert conn) entries

sqlUpsert :: SQL.Connection -> ArchiveEntry -> IO ()
sqlUpsert conn e = SQL.execute conn
  "INSERT OR REPLACE INTO archive_entries (id, code, parent_id, score, passed, generation, counter_ex) VALUES (?,?,?,?,?,?,?)"
  ( entryId e
  , entryCode e
  , entryParentId e
  , entryScore e
  , (if entryPassed e then 1 else 0 :: Int)
  , entryGeneration e
  , fmap (TE.decodeUtf8 . BL.toStrict . Aeson.encode) (entryCounterEx e)
  )

-- | Load all rows from the archive table.
sqlLoad :: SQL.Connection -> IO [ArchiveEntry]
sqlLoad conn = do
  rows <- SQL.query_ conn
    "SELECT id, code, parent_id, score, passed, generation, counter_ex FROM archive_entries"
  pure (map rowToEntry rows)
  where
    rowToEntry :: (Text, Text, Maybe Text, Double, Int, Int, Maybe Text) -> ArchiveEntry
    rowToEntry (eid, code, parentId, score, passed, gen, cex) = ArchiveEntry
      { entryId         = eid
      , entryCode       = code
      , entryMutation   = Nothing
      , entryParentId   = parentId
      , entryScore      = score
      , entryPassed       = passed /= (0 :: Int)
      , entryGeneration   = gen
      , entryCounterEx    = cex >>= Aeson.decode . BL.fromStrict . TE.encodeUtf8
      , entryLiquidResult = Nothing
      , entrySbvResult    = Nothing
      , entryOracleModel  = Nothing
      }

sqlCreateBlacklistTable :: SQL.Query
sqlCreateBlacklistTable =
  "CREATE TABLE IF NOT EXISTS mutation_blacklist (file_path TEXT NOT NULL, mutation_desc TEXT NOT NULL, PRIMARY KEY (file_path, mutation_desc))"

sqlFlushBlacklist :: SQL.Connection -> [(FilePath, Text)] -> IO ()
sqlFlushBlacklist conn pairs = SQL.withTransaction conn $
  mapM_ (\(fp, desc) ->
    SQL.execute conn
      "INSERT OR IGNORE INTO mutation_blacklist (file_path, mutation_desc) VALUES (?,?)"
      (fp, desc)) pairs

sqlLoadBlacklist :: SQL.Connection -> IO [(FilePath, Text)]
sqlLoadBlacklist conn = do
  rows <- SQL.query_ conn
    "SELECT file_path, mutation_desc FROM mutation_blacklist"
  pure rows

sqlCreateDynamicRulesTable :: SQL.Query
sqlCreateDynamicRulesTable =
  "CREATE TABLE IF NOT EXISTS dynamic_rules (description TEXT PRIMARY KEY, snippet TEXT NOT NULL, score REAL NOT NULL)"

sqlFlushDynamicRules :: SQL.Connection -> [(Text, Text, Double)] -> IO ()
sqlFlushDynamicRules conn triples = SQL.withTransaction conn $
  mapM_ (\(desc, snip, sc) ->
    SQL.execute conn
      "INSERT OR REPLACE INTO dynamic_rules (description, snippet, score) VALUES (?,?,?)"
      (desc, snip, sc)) triples

sqlLoadDynamicRules :: SQL.Connection -> IO [(Text, Text, Double)]
sqlLoadDynamicRules conn = do
  rows <- SQL.query_ conn
    "SELECT description, snippet, score FROM dynamic_rules"
  pure rows

#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | DGM.Oracle — LLM-based mutation proposer via OpenRouter (SI-6kx).
--
-- Provides 'OracleEnv' configuration and three principal operations:
--
--   * 'proposeMutation' — asks the LLM to generate a Haskell diff (the
--     genuine intelligence layer).  The diff is wrapped as an 'HsMutation'
--     whose 'hmTransform' applies it via text-level line replacement.
--
--   * 'scoreAndRankMutations' — scores a list of pre-generated mutations.
--     Useful when 'proposeMutation' would be too expensive.
--
--   * 'oracleHealthCheck' — verifies the API endpoint is reachable.
--
-- __Build flag__: compile with @-f+with-oracle@ to enable real HTTP calls.
-- Without that flag every oracle function returns a safe stub so the
-- pipeline remains functional without network access.
--
-- __Fallback__: when no API key is present (env var or @.env@ file), callers
-- receive @Nothing@ from 'newOracleEnv' and should fall back to heuristic
-- mutations.
--
-- __Key resolution order__:
--
--   1. The @OPENROUTER_API_KEY@ process environment variable.
--   2. The @.env@ file at the repository root (@KEY=VALUE@ format).
module DGM.Oracle
  ( -- * Environment
    OracleEnv(..)
  , newOracleEnv
    -- * Proposal
  , proposeMutation
  , MutationContext(..)
    -- * Scoring
  , scoreAndRankMutations
    -- * Health
  , oracleHealthCheck
    -- * .env parsing (exposed for testing)
  , parseDotEnv
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.Directory (doesFileExist)
import Control.Exception (try, SomeException)
import Data.Maybe (mapMaybe)

import DGM.HsAST (HsMutation(..))

#ifdef WITH_ORACLE
import Control.Monad (foldM)
import Data.Aeson ((.:), (.=), object, encode, withObject)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Simple as HTTP
#endif

-- | Context from the archive to guide the oracle toward high-value mutations.
data MutationContext = MutationContext
  { mcBestScore       :: Double     -- ^ Best score achieved so far.
  , mcTotalEntries    :: Int        -- ^ Total archive entries.
  , mcPassRate        :: Double     -- ^ Fraction of mutations that passed tests.
  , mcRecentSuccesses :: [Text]     -- ^ Last 3 successful mutation descriptions.
  , mcEvolutionGoal   :: Maybe Text -- ^ Natural-language goal if set.
  } deriving (Show, Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- Environment
-- ─────────────────────────────────────────────────────────────────────────────

-- | Runtime configuration for the LLM oracle.
--
-- Constructed by 'newOracleEnv' from environment variables.  The three fields
-- map directly onto the OpenRouter API contract:
--
--   * @oeApiKey@  — the secret key for the Authorization header.
--   * @oeModel@   — the model identifier (e.g. @"google/gemini-flash-1.5-8b"@).
--   * @oeBaseUrl@ — the API base URL (overridable for testing / proxies).
data OracleEnv = OracleEnv
  { oeApiKey  :: !Text  -- ^ @OPENROUTER_API_KEY@ value.
  , oeModel   :: !Text  -- ^ LLM model identifier.
  , oeBaseUrl :: !Text  -- ^ API base URL.
  } deriving (Show, Eq)

-- | Build an 'OracleEnv' from the process environment or a @.env@ file.
--
-- Reads @OPENROUTER_API_KEY@ from the environment variable first; if absent,
-- falls back to the @.env@ file at the current working directory (repo root).
-- Returns @Just env@ when a key is found, @Nothing@ otherwise.
-- Callers should treat @Nothing@ as "oracle unavailable" and fall back to
-- heuristic mutations.
newOracleEnv :: IO (Maybe OracleEnv)
newOracleEnv = do
  mKey <- lookupEnv "OPENROUTER_API_KEY"
  case mKey of
    Just k  -> return $ Just (mkEnv (T.pack k))
    Nothing -> do
      mDotEnvKey <- readDotEnvKey "OPENROUTER_API_KEY"
      return $ fmap mkEnv mDotEnvKey
  where
    mkEnv k = OracleEnv
      { oeApiKey  = k
      , oeModel   = "google/gemini-3-flash-preview"
      , oeBaseUrl = "https://openrouter.ai/api/v1"
      }

-- | Read a single key's value from the @.env@ file at @./.env@.
--
-- Returns @Nothing@ if the file does not exist or the key is absent.
readDotEnvKey :: String -> IO (Maybe Text)
readDotEnvKey key = do
  exists <- doesFileExist ".env"
  if not exists
    then return Nothing
    else do
      eContent <- try (TIO.readFile ".env") :: IO (Either SomeException Text)
      let content = case eContent of { Left _ -> ""; Right t -> t }
      return $ lookup (T.pack key) (parseDotEnv content)

-- | Parse a @.env@ file body into @(key, value)@ pairs.
--
-- Blank lines and lines starting with @#@ are ignored.  Key names are
-- stripped of surrounding whitespace.  Values are returned verbatim
-- (everything after the first @=@, unquoted).
parseDotEnv :: Text -> [(Text, Text)]
parseDotEnv = mapMaybe parseLine . T.lines
  where
    parseLine line =
      let stripped = T.strip line
      in if T.null stripped || "#" `T.isPrefixOf` stripped
           then Nothing
           else case T.breakOn "=" stripped of
                  (k, rest) | not (T.null rest) ->
                    Just (T.strip k, T.drop 1 rest)
                  _ -> Nothing

-- ─────────────────────────────────────────────────────────────────────────────

#ifdef WITH_ORACLE

-- ─────────────────────────────────────────────────────────────────────────────
-- Real HTTP implementation (with-oracle flag enabled)
-- ─────────────────────────────────────────────────────────────────────────────

-- | System role message sent with every mutation-proposal request.
oracleSystemPrompt :: Text
oracleSystemPrompt =
  "You are a mutation oracle for DGM, a self-improving Haskell system. " <>
  "Propose exactly ONE small, safe improvement to the given Haskell module " <>
  "as a unified diff. Valued improvements include: removing unused imports, " <>
  "simplifying expressions, adding type signatures, improving documentation, " <>
  "reducing code duplication, and eliminating dead code. " <>
  "Output ONLY the diff, starting with --- lines and ending after the last " <>
  "+++ block. Do not include any explanation or markdown fences. " <>
  "Do not include line numbers or the numbered source lines in your diff. " <>
  "Use the original source lines exactly as they appear (preserving indentation)."

-- | Build the user-role prompt for a mutation request.
buildMutationPrompt :: FilePath -> Text -> [Text] -> Maybe MutationContext -> Text
buildMutationPrompt fp src tests mCtx =
  T.unlines $
    [ "Target file: " <> T.pack fp
    , ""
    , "Current source (line numbers for reference only, do not include in diff):"
    , "---"
    ] ++ zipWith (\i l -> T.pack (show i) <> " " <> l) [1::Int ..] (T.lines src) ++
    [ "---"
    , ""
    ] ++
    ( if null tests then []
      else [ "Test functions: " <> T.intercalate ", " tests, "" ]
    ) ++
    archiveSection ++
    [ "Output ONE unified diff (--- / +++ format only, no commentary)." ]
  where
    archiveSection :: [Text]
    archiveSection = case mCtx of
      Nothing  -> []
      Just ctx ->
        let passPercent = T.pack (show (round (mcPassRate ctx * 100) :: Int))
            bestStr     = T.pack (show (mcBestScore ctx))
            totalStr    = T.pack (show (mcTotalEntries ctx))
            recentLines = case mcRecentSuccesses ctx of
              [] -> []
              ss -> [ "Recent successful mutations: "
                        <> T.intercalate "; " ss
                    , ""
                    ]
            goalLines = case mcEvolutionGoal ctx of
              Nothing -> []
              Just g  -> ["Evolution goal: " <> g, ""]
        in  [ "Archive statistics: " <> totalStr <> " entries, "
                <> passPercent <> "% pass rate, best score " <> bestStr <> "."
            , ""
            ] ++ recentLines ++ goalLines

-- | Internal type for parsing OpenRouter chat completion responses.
newtype OracleResponse = OracleResponse Text

instance Aeson.FromJSON OracleResponse where
  parseJSON = withObject "OracleResponse" $ \o -> do
    choices <- o .: "choices"
    case (choices :: [Aeson.Value]) of
      [] -> fail "oracle: empty choices array"
      (c:_) -> do
        msg     <- withObject "Choice"   (.: "message")  c
        content <- withObject "Message"  (.: "content")  msg
        return (OracleResponse content)

-- | Ask the oracle to propose a single 'HsMutation' for @fp@.
--
-- Sends a structured prompt to the configured LLM (via OpenRouter), parses
-- the unified diff in the response, and wraps it as an 'HsMutation' whose
-- 'hmTransform' applies the diff via text-level line replacement.
--
-- When a 'MutationContext' is provided the prompt includes archive statistics,
-- recent successes, and the evolution goal so the oracle can propose
-- higher-value mutations.
--
-- Returns @Right mutation@ on success, @Left reason@ on failure (including
-- network errors, parse errors, or empty diffs).
proposeMutation
  :: OracleEnv
  -> FilePath            -- ^ Target source file (relative to repo root).
  -> Text                -- ^ Full module source text.
  -> [Text]              -- ^ Test function names present in the module.
  -> Maybe MutationContext -- ^ Optional archive feedback for guided mutation.
  -> IO (Either Text HsMutation)
proposeMutation env fp src tests mCtx = do
  let url     = T.unpack (oeBaseUrl env) <> "/chat/completions"
      bodyLBS = encode $ object
                  [ "model"    .= oeModel env
                  , "temperature" .= (0.3 :: Double)
                  , "max_tokens"  .= (4096 :: Int)
                  , "messages" .= Aeson.toJSON
                      [ object [ "role"    .= ("system" :: Text)
                               , "content" .= oracleSystemPrompt ]
                      , object [ "role"    .= ("user" :: Text)
                               , "content" .= buildMutationPrompt fp src tests mCtx ]
                      ]
                  ]
  eReq <- try (HTTP.parseRequest url) :: IO (Either SomeException HTTP.Request)
  case eReq of
    Left err -> return (Left ("oracle: bad URL: " <> T.pack (show err)))
    Right req0 -> do
      let req = HTTP.setRequestMethod "POST"
              $ HTTP.addRequestHeader "Content-Type"  "application/json"
              $ HTTP.addRequestHeader "Authorization"
                  ("Bearer " <> BS8.pack (T.unpack (oeApiKey env)))
              $ HTTP.setRequestBodyLBS bodyLBS req0
      eResp <- try (HTTP.httpLBS req)
                 :: IO (Either SomeException (HTTP.Response LBS.ByteString))
      case eResp of
        Left err   -> return (Left ("oracle: HTTP error: " <> T.pack (show err)))
        Right resp -> return $ case Aeson.decode (HTTP.getResponseBody resp) of
          Nothing                   -> Left "oracle: JSON decode failed"
          Just (OracleResponse txt) -> parseDiffResponse txt

-- | Score and rank a list of pre-generated mutations via the oracle.
--
-- Returns each mutation paired with a confidence score in @[0, 1]@.
-- The current implementation assigns a uniform score of @0.5@; a future
-- improvement could batch-score via the LLM in a single request.
scoreAndRankMutations
  :: OracleEnv
  -> [HsMutation]
  -> IO [(HsMutation, Double)]
scoreAndRankMutations _env muts = return [(m, 0.5) | m <- muts]

-- | Verify that the oracle API endpoint is reachable.
--
-- Issues a GET to @{oeBaseUrl}/models@.  Returns @True@ on a 2xx response,
-- @False@ on any error or non-2xx status.
oracleHealthCheck :: OracleEnv -> IO Bool
oracleHealthCheck env = do
  let url = T.unpack (oeBaseUrl env) <> "/models"
  eReq <- try (HTTP.parseRequest url) :: IO (Either SomeException HTTP.Request)
  case eReq of
    Left _    -> return False
    Right req -> do
      let req' = HTTP.addRequestHeader "Authorization"
                   ("Bearer " <> BS8.pack (T.unpack (oeApiKey env))) req
      eResp <- try (HTTP.httpLBS req')
                 :: IO (Either SomeException (HTTP.Response LBS.ByteString))
      case eResp of
        Left  _    -> return False
        Right resp ->
          let sc = HTTP.getResponseStatusCode resp
          in  return (sc >= 200 && sc < 300)

-- ─────────────────────────────────────────────────────────────────────────────
-- Diff parsing and application (WITH_ORACLE only)
-- ─────────────────────────────────────────────────────────────────────────────

-- | A single hunk from a unified diff.
data DiffHunk = DiffHunk
  { hunkRemoves :: [Text]  -- ^ lines to remove (leading @-@ stripped)
  , hunkAdds    :: [Text]  -- ^ lines to add (leading @+@ stripped)
  , hunkContext :: [Text]  -- ^ space-prefixed context lines (leading space stripped)
  } deriving (Show, Eq)

-- | Split diff lines into hunks. Each hunk starts at a @\@\@ ... \@\@@ header line.
-- Everything before the first @\@\@ @ line is discarded (it's the --- / +++ header).
splitIntoHunks :: [Text] -> [DiffHunk]
splitIntoHunks ls =
  let afterHeader = dropWhile (\l -> not ("@@ " `T.isPrefixOf` l)) ls
  in  parseGroups afterHeader
  where
    parseGroups [] = []
    parseGroups (_h:rest) =
      -- _h is the @@ header line; collect body until next @@
      let (body, remainder) = break ("@@ " `T.isPrefixOf`) rest
          hunk = toHunk body
      in  hunk : parseGroups remainder

    toHunk bodyLines = DiffHunk
      { hunkRemoves = [ T.drop 1 l | l <- bodyLines
                                    , "-" `T.isPrefixOf` l
                                    , not ("---" `T.isPrefixOf` l) ]
      , hunkAdds    = [ T.drop 1 l | l <- bodyLines
                                    , "+" `T.isPrefixOf` l
                                    , not ("+++" `T.isPrefixOf` l) ]
      , hunkContext = [ T.drop 1 l | l <- bodyLines
                                    , " " `T.isPrefixOf` l ]
      }

-- | Apply a single hunk to source lines.
-- For hunks with removes: find removes as contiguous block, replace with adds.
-- For insert-only hunks with context: find context block, insert adds after it.
-- For insert-only hunks without context: append adds at end.
applyHunk :: DiffHunk -> [Text] -> Either Text [Text]
applyHunk DiffHunk{..} srcLines
  | null hunkRemoves && null hunkContext =
      -- Insert-only with no context: append at end
      Right (srcLines ++ hunkAdds)
  | null hunkRemoves =
      -- Insert-only with context: find context block, insert adds after it
      case findSubseq hunkContext srcLines of
        Nothing  -> Right (srcLines ++ hunkAdds)  -- fallback: append
        Just idx ->
          let before = take (idx + length hunkContext) srcLines
              after  = drop (idx + length hunkContext) srcLines
          in Right (before ++ hunkAdds ++ after)
  | otherwise =
      case findSubseq hunkRemoves srcLines of
        Just idx ->
          let before = take idx srcLines
              after  = drop (idx + length hunkRemoves) srcLines
          in Right (before ++ hunkAdds ++ after)
        Nothing  -> Left "oracle diff: hunk removes not found in source"

-- | Parse a unified diff from an LLM text response into an 'HsMutation'.
--
-- Splits the diff into per-hunk structures and applies them sequentially,
-- so multi-hunk diffs work correctly.
parseDiffResponse :: Text -> Either Text HsMutation
parseDiffResponse content =
  let stripped = stripMarkdownFences content
      ls    = T.lines stripped
      hunks = splitIntoHunks ls
      nonEmpty = filter (\h -> not (null (hunkRemoves h)) || not (null (hunkAdds h))) hunks
  in  if null nonEmpty
        then Left "oracle: no hunks found in diff response"
        else Right HsMutation
               { hmDescription = "oracle-diff: " <> T.take 50 (T.strip stripped)
               , hmTransform   = applyAllHunks nonEmpty
               }
  where
    applyAllHunks :: [DiffHunk] -> Text -> Either Text Text
    applyAllHunks hs src = fmap T.unlines $ foldM applyOneHunk (T.lines src) hs
      where
        applyOneHunk acc h = applyHunk h acc

-- | Apply a text-level line-replacement diff to module source (backward compat).
--
-- Wraps the given removes\/adds as a single 'DiffHunk' and applies it.
applyLineDiff :: [Text] -> [Text] -> Text -> Either Text Text
applyLineDiff removes adds src =
  let hunk = DiffHunk { hunkRemoves = removes, hunkAdds = adds, hunkContext = [] }
  in  fmap T.unlines $ applyHunk hunk (T.lines src)

-- | Find the first index of @needle@ as a contiguous sublist of @haystack@.
-- Two-pass strategy: first tries trailing-whitespace-normalized match, then
-- falls back to a loose match that strips all leading/trailing whitespace
-- (handles LLM diffs with slightly different indentation).
findSubseq :: [Text] -> [Text] -> Maybe Int
findSubseq needle haystack =
  case findSubseqExact needle haystack of
    Just idx -> Just idx
    Nothing  -> findSubseqLoose needle haystack
  where
    findSubseqExact ns hs = go 0 hs
      where
        n' = length ns
        normE = T.stripEnd
        nsNorm = map normE ns
        go _ [] = Nothing
        go i xs
          | map normE (take n' xs) == nsNorm = Just i
          | otherwise = go (i + 1) (tail xs)
    findSubseqLoose ns hs = go 0 hs
      where
        n' = length ns
        normL = T.strip
        nsNorm = map normL ns
        go _ [] = Nothing
        go i xs
          | map normL (take n' xs) == nsNorm = Just i
          | otherwise = go (i + 1) (tail xs)

-- | Strip markdown fences (@```diff@, @```haskell@, @```@ etc.) from LLM output.
stripMarkdownFences :: Text -> Text
stripMarkdownFences t =
  let ls = T.lines t
      -- Drop leading fence line if present
      ls1 = case ls of
              (h:rest) | "```" `T.isPrefixOf` T.strip h -> rest
              _ -> ls
      -- Drop trailing fence line if present
      ls2 = case reverse ls1 of
              (h:rest) | "```" `T.isPrefixOf` T.strip h -> reverse rest
              _ -> ls1
  in T.unlines ls2

#else

-- ─────────────────────────────────────────────────────────────────────────────
-- Stubs (with-oracle flag absent)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Ask the oracle to propose a single 'HsMutation' for @fp@.
--
-- __Stub behaviour__: always returns a @Left@ explanation.  Build with
-- @-f+with-oracle@ to enable real HTTP calls.
proposeMutation
  :: OracleEnv
  -> FilePath              -- ^ Target source file (relative to repo root).
  -> Text                  -- ^ Full module source text.
  -> [Text]                -- ^ Test function names present in the module.
  -> Maybe MutationContext -- ^ Optional archive feedback for guided mutation.
  -> IO (Either Text HsMutation)
proposeMutation _env _fp _src _tests _mCtx =
  return (Left "DGM.Oracle: build with -f+with-oracle to enable LLM mutations")

-- | Score and rank mutations.
--
-- __Stub behaviour__: assigns a uniform score of @0.5@ to every mutation.
scoreAndRankMutations
  :: OracleEnv
  -> [HsMutation]
  -> IO [(HsMutation, Double)]
scoreAndRankMutations _env muts = return [(m, 0.5) | m <- muts]

-- | Verify that the oracle API endpoint is reachable.
--
-- __Stub behaviour__: always returns @False@ (no HTTP client available).
oracleHealthCheck :: OracleEnv -> IO Bool
oracleHealthCheck _env = return False

#endif

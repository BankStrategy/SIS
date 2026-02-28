{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | DGM.NatLang — natural-language goal steering for the evolution loop (SI-2fy).
--
-- Provides a small DSL for expressing high-level mutation goals in plain
-- English and translating them into a structured 'EvolutionGoal' that the
-- SelfMod cycle can use to filter candidate source files.
--
-- The pipeline:
--
--   1. 'parseGoal' sends the NL text to the Oracle (when available) and
--      asks it to return a JSON array of 'MutationConstraint' values.
--      Without an oracle key (or when parsing fails), it falls back to a
--      single 'GoalText' constraint wrapping the raw input.
--
--   2. 'applyGoal' filters a list of 'FilePath' candidates using the
--      'PreferModule' and 'AvoidModule' constraints in the goal.
--      Example: @"improve Archive"@ → keeps only @src/DGM/Archive.hs@.
--
--   3. 'describeGoal' formats the goal as a human-readable summary line
--      suitable for printing in the startup banner.
--
-- __Build flag__: compile with @-f+with-oracle@ to enable LLM-powered
-- constraint parsing.  Without that flag 'parseGoal' returns a single
-- 'GoalText' constraint and the pipeline remains functional.
module DGM.NatLang
  ( -- * Constraint types
    MutationConstraint(..)
  , EvolutionGoal(..)
    -- * Goal operations
  , parseGoal
  , applyGoal
  , describeGoal
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeFileName)

import qualified DGM.Oracle as Oracle

#ifdef WITH_ORACLE
import Data.Aeson (withObject, (.:), (.=), object, encode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as LBS
import qualified Network.HTTP.Simple   as HTTP
import Control.Exception (try, SomeException)
#endif

-- ─────────────────────────────────────────────────────────────────────────────
-- Types
-- ─────────────────────────────────────────────────────────────────────────────

-- | A single constraint on the mutation search space.
--
-- 'PreferModule' and 'AvoidModule' are the primary filters; the others
-- are reserved for future pipeline stages.
data MutationConstraint
  = PreferModule Text   -- ^ Focus mutations on this module name (partial match on basename).
  | AvoidModule  Text   -- ^ Exclude this module from candidates (partial match on basename).
  | MaxImpact    Int    -- ^ Limit mutations to those touching ≤ N lines.
  | RequireTests        -- ^ Only propose mutations that have associated tests.
  | GoalText     Text   -- ^ Raw natural-language goal (fallback / pass-through).
  deriving (Show, Eq)

-- | A structured evolution goal derived from a natural-language description.
data EvolutionGoal = EvolutionGoal
  { egDescription :: Text                  -- ^ Original NL description.
  , egConstraints :: [MutationConstraint]  -- ^ Parsed constraint list.
  } deriving (Show)

-- ─────────────────────────────────────────────────────────────────────────────
-- parseGoal
-- ─────────────────────────────────────────────────────────────────────────────

#ifdef WITH_ORACLE

-- | Parse a natural-language goal description into a structured 'EvolutionGoal'.
--
-- Sends the text to the LLM oracle with a structured prompt that asks for a
-- JSON array of 'MutationConstraint' values.  On any failure (network error,
-- JSON parse error, empty response) falls back to
-- @EvolutionGoal { egDescription = input, egConstraints = [GoalText input] }@.
--
-- __Build flag__: requires @-f+with-oracle@.  Without it the stub below is
-- compiled instead.
parseGoal :: Oracle.OracleEnv -> Text -> IO EvolutionGoal
parseGoal env input = do
  let url      = T.unpack (Oracle.oeBaseUrl env) <> "/chat/completions"
      bodyLBS  = encode $ object
                   [ "model"    .= Oracle.oeModel env
                   , "messages" .= Aeson.toJSON
                       [ object [ "role"    .= ("system" :: Text)
                                , "content" .= goalSystemPrompt ]
                       , object [ "role"    .= ("user" :: Text)
                                , "content" .= ("Evolution goal: " <> input) ]
                       ]
                   ]
  eReq <- try (HTTP.parseRequest url) :: IO (Either SomeException HTTP.Request)
  case eReq of
    Left _ -> return (fallback input)
    Right req0 -> do
      let req = HTTP.setRequestMethod "POST"
              $ HTTP.addRequestHeader "Content-Type"  "application/json"
              $ HTTP.addRequestHeader "Authorization"
                  ("Bearer " <> BS8.pack (T.unpack (Oracle.oeApiKey env)))
              $ HTTP.setRequestBodyLBS bodyLBS req0
      eResp <- try (HTTP.httpLBS req)
                 :: IO (Either SomeException (HTTP.Response LBS.ByteString))
      case eResp of
        Left  _ -> return (fallback input)
        Right resp ->
          case Aeson.decode (HTTP.getResponseBody resp) of
            Nothing                   -> return (fallback input)
            Just (GoalResponse content) ->
              case Aeson.eitherDecode (LBS.fromStrict (BS8.pack (T.unpack content))) of
                Left  _ -> return (fallback input)
                Right cs -> return EvolutionGoal
                              { egDescription = input
                              , egConstraints = cs
                              }

-- | Internal type for extracting the assistant's reply from an OpenRouter
-- chat completion response.
newtype GoalResponse = GoalResponse Text

instance Aeson.FromJSON GoalResponse where
  parseJSON = withObject "GoalResponse" $ \o -> do
    choices <- o .: "choices"
    case (choices :: [Aeson.Value]) of
      []    -> fail "oracle: empty choices"
      (c:_) -> do
        msg     <- withObject "Choice"  (.: "message") c
        content <- withObject "Message" (.: "content") msg
        return (GoalResponse content)

instance Aeson.FromJSON MutationConstraint where
  parseJSON = withObject "MutationConstraint" $ \o -> do
    typ <- o .: "type"
    case (typ :: Text) of
      "PreferModule" -> PreferModule <$> o .: "name"
      "AvoidModule"  -> AvoidModule  <$> o .: "name"
      "MaxImpact"    -> MaxImpact    <$> o .: "value"
      "RequireTests" -> pure RequireTests
      "GoalText"     -> GoalText     <$> o .: "text"
      other          -> fail ("unknown constraint type: " <> T.unpack other)

-- | System prompt for the goal-parsing oracle call.
goalSystemPrompt :: Text
goalSystemPrompt =
  "You are a mutation steering oracle for DGM, a self-improving Haskell system. " <>
  "Given a natural-language evolution goal, return ONLY a JSON array of constraint " <>
  "objects with no explanation. Each object must have a 'type' field set to one of: " <>
  "'PreferModule' (with 'name' string), 'AvoidModule' (with 'name' string), " <>
  "'MaxImpact' (with 'value' integer), 'RequireTests', 'GoalText' (with 'text' string). " <>
  "Example: [{\"type\":\"PreferModule\",\"name\":\"Archive\"}]"

#else

-- | Parse a natural-language goal description into a structured 'EvolutionGoal'.
--
-- __Stub behaviour__: ignores the 'Oracle.OracleEnv' and returns
-- @EvolutionGoal { egDescription = input, egConstraints = [GoalText input] }@.
-- Build with @-f+with-oracle@ to enable LLM-powered constraint parsing.
parseGoal :: Oracle.OracleEnv -> Text -> IO EvolutionGoal
parseGoal _env input = return (fallback input)

#endif

-- | Fallback goal used when oracle is unavailable or parsing fails.
fallback :: Text -> EvolutionGoal
fallback input = EvolutionGoal
  { egDescription = input
  , egConstraints = [GoalText input]
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- applyGoal
-- ─────────────────────────────────────────────────────────────────────────────

-- | Filter a list of candidate 'FilePath' values using the constraints in
-- the 'EvolutionGoal'.
--
-- * 'PreferModule' @name@ — retain only files whose basename contains @name@
--   (case-insensitive partial match).
-- * 'AvoidModule'  @name@ — remove all files whose basename contains @name@
--   (case-insensitive partial match).
-- * Other constraints ('MaxImpact', 'RequireTests', 'GoalText') are ignored
--   by this filter (they are applied elsewhere in the pipeline).
--
-- If no 'PreferModule' constraints are present, all files pass the preference
-- filter.  'AvoidModule' constraints are always applied.
--
-- Examples:
--
-- > applyGoal (EvolutionGoal "improve Archive" [PreferModule "Archive"]) allSrcs
-- > -- → ["src/DGM/Archive.hs"]
--
-- > applyGoal (EvolutionGoal "avoid Types" [AvoidModule "Types"]) allSrcs
-- > -- → allSrcs \\ ["src/DGM/Types.hs"]
applyGoal :: EvolutionGoal -> [FilePath] -> [FilePath]
applyGoal goal fps =
  let cs      = egConstraints goal
      prefs   = [n | PreferModule n <- cs]
      avoids  = [n | AvoidModule  n <- cs]
      noAvoid = filter (not . matchesAny avoids) fps
  in if null prefs
       then noAvoid
       else filter (matchesAny prefs) noAvoid

-- | True when any of the name fragments match the filepath basename
-- (case-insensitive).
matchesAny :: [Text] -> FilePath -> Bool
matchesAny names fp =
  let base = T.toLower (T.pack (takeFileName fp))
  in any (\n -> T.toLower n `T.isInfixOf` base) names

-- ─────────────────────────────────────────────────────────────────────────────
-- describeGoal
-- ─────────────────────────────────────────────────────────────────────────────

-- | Produce a human-readable one-line summary of the goal.
--
-- Format: @"Goal: \<description\> [\<N\> constraint(s) active]"@
describeGoal :: EvolutionGoal -> Text
describeGoal goal =
  let n = length (egConstraints goal)
      c | n == 1    = "1 constraint active"
        | otherwise = T.pack (show n) <> " constraints active"
  in "Goal: " <> egDescription goal <> " [" <> c <> "]"

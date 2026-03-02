{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | DGM.AgentOracle — Multi-turn agentic tool-use loop for self-modification.
--
-- Transforms the Oracle from a single-shot LLM call into a multi-turn agent
-- that can explore the codebase, test changes, and iterate before committing.
-- Uses OpenRouter's tool-use wire format (OpenAI-compatible).
--
-- Key design: @submit_change@ applies diffs /live/ — if build+test passes,
-- the change is kept on disk and the agent continues.  Multiple changes can
-- be submitted in a single session, each building on the previous state.
--
-- __Build flag__: compile with @-f+with-oracle@ to enable.  Without the flag
-- every function returns a safe stub.
module DGM.AgentOracle
  ( -- * Configuration
    AgentConfig(..)
    -- * Result
  , AgentResult(..)
    -- * Agent loop
  , runAgentOracle
    -- * Tool definitions (for testing)
  , agentTools
#ifdef WITH_ORACLE
    -- * Internal types (for testing)
  , ToolCall(..)
  , ToolCallFunction(..)
  , AgentResponse(..)
  , parseAgentResponse
  , parseToolArgs
  , mkTool
#endif
  ) where

-- ─────────────────────────────────────────────────────────────────────────────
-- Imports
-- ─────────────────────────────────────────────────────────────────────────────

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

import DGM.HsAST (HsMutation(..))

#ifdef WITH_ORACLE
import Control.Exception (SomeException, try, catch, finally)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson.Types as Aeson (parseEither, Parser)
import qualified Data.Aeson.Key as AK
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Simple as HTTP
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)

import DGM.SelfCompile (buildPreflight, testSelf, CompileResult(..))
import DGM.SemanticContext (extractFunctions, FunctionInfo(..))
#endif

-- ─────────────────────────────────────────────────────────────────────────────
-- Types (always available)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Configuration for the agent oracle (avoids circular import with Oracle).
data AgentConfig = AgentConfig
  { acApiKey  :: !Text  -- ^ OpenRouter API key.
  , acModel   :: !Text  -- ^ LLM model identifier.
  , acBaseUrl :: !Text  -- ^ API base URL.
  } deriving (Show, Eq)

-- | Result from the agent oracle loop.
data AgentResult = AgentResult
  { arChangesApplied :: !Int        -- ^ Successful submit_change calls.
  , arModifiedFiles  :: [FilePath]  -- ^ Files modified and kept on disk.
  , arTurnsUsed      :: !Int        -- ^ LLM round-trips consumed.
  } deriving (Show, Eq)

#ifdef WITH_ORACLE

-- ─────────────────────────────────────────────────────────────────────────────
-- Internal types
-- ─────────────────────────────────────────────────────────────────────────────

-- | A tool call from the LLM response.
data ToolCall = ToolCall
  { tcId       :: !Text
  , tcFunction :: !ToolCallFunction
  } deriving (Show, Eq)

data ToolCallFunction = ToolCallFunction
  { tcfName      :: !Text
  , tcfArguments :: !Text   -- ^ JSON string, parsed per-tool.
  } deriving (Show, Eq)

-- | Parsed response from OpenRouter.
data AgentResponse
  = AgentToolCalls Text [ToolCall]   -- ^ Assistant content + tool calls.
  | AgentFinalContent Text           -- ^ Final text content (no tool calls).
  | AgentStopNoContent               -- ^ LLM stopped without content or tool calls.
  deriving (Show, Eq)

-- | Type alias for the diff parser (passed in to avoid circular imports).
type DiffParser = Text -> Either Text (Maybe FilePath, HsMutation)

-- | Parsed tool arguments as a simple map.
type ToolArgs = Map.Map Text Aeson.Value

-- | Loop state threaded through the agent loop.
data LoopState = LoopState
  { lsMessages       :: [Aeson.Value]
  , lsTurn           :: !Int
  , lsBuildOps       :: !Int
  , lsChangesApplied :: !Int
  , lsModifiedFiles  :: [FilePath]
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- JSON parsing
-- ─────────────────────────────────────────────────────────────────────────────

parseToolCallJSON :: Aeson.Value -> Aeson.Parser ToolCall
parseToolCallJSON = Aeson.withObject "ToolCall" $ \o -> do
  tcId'  <- o .: "id"
  fn     <- o .: "function"
  name   <- Aeson.withObject "Function" (.: "name") fn
  mArgs  <- Aeson.withObject "Function" (.:? "arguments") fn
  let args = maybe "{}" id mArgs
  return ToolCall { tcId = tcId', tcFunction = ToolCallFunction name args }

parseAgentResponse :: LBS.ByteString -> Either Text AgentResponse
parseAgentResponse body = case Aeson.decode body of
  Nothing  -> Left "agent: JSON decode failed"
  Just val -> case Aeson.parseEither parseResp val of
    Left err  -> Left (T.pack err)
    Right res -> Right res
  where
    parseResp = Aeson.withObject "Response" $ \o -> do
      choices <- o .: "choices"
      case (choices :: [Aeson.Value]) of
        [] -> fail "agent: empty choices"
        (c:_) -> Aeson.withObject "Choice" (\ch -> do
          msg <- ch .: "message"
          Aeson.withObject "Message" (\m -> do
            mContent   <- m .:? "content"
            mToolCalls <- m .:? "tool_calls"
            case (mToolCalls :: Maybe [Aeson.Value]) of
              Just tcs | not (null tcs) -> do
                parsed <- mapM parseToolCallJSON tcs
                return $ AgentToolCalls (maybe "" id mContent) parsed
              _ -> case mContent of
                Just txt | not (T.null (T.strip txt)) ->
                  return $ AgentFinalContent txt
                _ -> return AgentStopNoContent
            ) msg
          ) c

-- ─────────────────────────────────────────────────────────────────────────────
-- Tool argument parsing
-- ─────────────────────────────────────────────────────────────────────────────

parseToolArgs :: Text -> ToolArgs
parseToolArgs argsJson =
  case Aeson.decode (LBS.fromStrict (encodeUtf8 argsJson)) of
    Just m  -> m
    Nothing -> Map.empty

argText :: ToolArgs -> Text -> Text
argText m key = case Map.lookup key m of
  Just (Aeson.String v) -> v
  _                     -> ""

argInt :: ToolArgs -> Text -> Maybe Int
argInt m key = case Map.lookup key m of
  Just (Aeson.Number n) -> Just (round n)
  _                     -> Nothing

-- ─────────────────────────────────────────────────────────────────────────────
-- Tool definitions (OpenRouter/OpenAI wire format)
-- ─────────────────────────────────────────────────────────────────────────────

agentTools :: [Aeson.Value]
agentTools =
  [ mkTool "read_file" "Read a source file's contents (with optional line range)"
      [("path", "string", "File path relative to repo root", True)
      ,("start_line", "integer", "First line to read (1-based, optional)", False)
      ,("end_line", "integer", "Last line to read (1-based, optional)", False)]
  , mkTool "list_files" "List files in a directory, optionally filtered by extension"
      [("directory", "string", "Directory path relative to repo root", True)
      ,("extension", "string", "Filter by extension e.g. '.hs' (optional)", False)]
  , mkTool "search_code" "Search for a text pattern across Haskell source files"
      [("pattern", "string", "Text pattern to search for", True)
      ,("directory", "string", "Directory to search in (default: whole project)", False)]
  , mkTool "get_module_info" "Get type signatures, exports, and complexity for a module"
      [("path", "string", "Path to the Haskell source file", True)]
  , mkTool "get_test_file" "Read the current test/Spec.hs content"
      []
  , mkTool "run_build" "Run cabal build and return success/failure with error details"
      []
  , mkTool "run_tests" "Run cabal test and return pass/fail details"
      []
  , mkTool "try_change" "DRY RUN: apply a unified diff, build+test, then REVERT. Returns results without keeping changes"
      [("diff", "string", "The unified diff to try (must include --- a/ and +++ b/ headers)", True)]
  , mkTool "submit_change" "APPLY LIVE: apply a unified diff, build+test. If all tests pass the change is KEPT on disk. If tests fail the change is reverted. You can continue making more changes after this."
      [("diff", "string", "The unified diff to apply", True)]
  , mkTool "finish" "Signal that you have completed all work. Call this when the task is fully done."
      [("summary", "string", "Summary of all changes made", True)]
  ]

mkTool :: Text -> Text -> [(Text, Text, Text, Bool)] -> Aeson.Value
mkTool name desc params = Aeson.object
  [ "type" .= ("function" :: Text)
  , "function" .= Aeson.object
    [ "name" .= name
    , "description" .= desc
    , "parameters" .= Aeson.object
      ( [ "type" .= ("object" :: Text)
        , "properties" .= Aeson.object
          [ (AK.fromText pname, Aeson.object
              [ "type" .= ptype
              , "description" .= pdesc
              ])
          | (pname, ptype, pdesc, _) <- params
          ]
        ] ++
        [ "required" .= [pname | (pname, _, _, True) <- params]
        | not (null [() | (_, _, _, True) <- params])
        ]
      )
    ]
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- System prompt
-- ─────────────────────────────────────────────────────────────────────────────

agentSystemPrompt :: Text
agentSystemPrompt = T.unlines
  [ "You are an expert Haskell engineer working on a self-improving system called DGM."
  , "You have tools to explore the codebase, understand module structure, test changes,"
  , "and apply modifications live. You are capable of handling ambitious tasks including"
  , "large-scale architecture changes, new features, and multi-file refactors."
  , ""
  , "WORKFLOW:"
  , "1. Study the task and explore the codebase thoroughly using read_file, list_files,"
  , "   search_code, and get_module_info. Understand the architecture before changing anything."
  , "2. Plan your approach — break large tasks into incremental steps."
  , "3. Implement changes one at a time using submit_change. Each successful submission"
  , "   persists on disk immediately — subsequent reads see the updated code."
  , "4. Use try_change for dry-runs when you want to test a diff without committing it."
  , "5. After each submit_change, verify the change took effect by reading the file."
  , "6. Continue making changes until the task is fully complete."
  , "7. Call finish when you are done with ALL work."
  , ""
  , "DIFF FORMAT:"
  , "- Always include --- a/<path> and +++ b/<path> headers"
  , "- Use unified diff format with @@ hunk headers"
  , "- Match existing code style exactly (indentation, naming conventions)"
  , "- For test changes, target test/Spec.hs"
  , ""
  , "GUIDELINES:"
  , "- Break large changes into small, testable increments"
  , "- Each submit_change should leave the codebase in a working state"
  , "- If a submit fails, read the error, fix your diff, and try again"
  , "- You can modify multiple files across multiple submit_change calls"
  , "- Re-read files after changes to see the updated state"
  , "- Do not stop until the task is fully implemented and all tests pass"
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Constants
-- ─────────────────────────────────────────────────────────────────────────────

-- | Maximum LLM round-trips before stopping.  High enough for ambitious tasks.
maxTurns :: Int
maxTurns = 50

-- | Maximum build+test operations (try_change + submit_change) per session.
maxBuildOps :: Int
maxBuildOps = 20

-- ─────────────────────────────────────────────────────────────────────────────
-- Core agent loop
-- ─────────────────────────────────────────────────────────────────────────────

-- | Run the agentic oracle loop.
--
-- The agent explores the codebase using tools, then applies changes live via
-- @submit_change@.  Each successful submission persists on disk.  The loop
-- ends when the agent responds with text (no tool calls), max turns are
-- reached, or an API error occurs.
--
-- Returns 'AgentResult' with the number of changes applied and files modified.
-- The caller can read the modified files to create a pipeline-compatible
-- mutation.
runAgentOracle
  :: AgentConfig
  -> DiffParser          -- ^ Diff parser (passed in to avoid circular imports).
  -> FilePath            -- ^ Target file being analyzed.
  -> Text                -- ^ Module source code.
  -> Text                -- ^ Semantic prompt (context).
  -> IO (Either Text AgentResult)
runAgentOracle cfg diffParser targetFp src semanticPrompt = do
  hPutStrLn stderr "[agent] starting agentic oracle loop"
  let systemMsg = Aeson.object
        [ "role"    .= ("system" :: Text)
        , "content" .= agentSystemPrompt
        ]
      userMsg = Aeson.object
        [ "role"    .= ("user" :: Text)
        , "content" .= buildInitialUserMessage targetFp src semanticPrompt
        ]
      initState = LoopState
        { lsMessages       = [systemMsg, userMsg]
        , lsTurn           = 0
        , lsBuildOps       = 0
        , lsChangesApplied = 0
        , lsModifiedFiles  = []
        }
  loop initState
  where
    loop :: LoopState -> IO (Either Text AgentResult)
    loop ls
      | lsTurn ls >= maxTurns = do
          hPutStrLn stderr $ "[agent] max turns (" ++ show maxTurns ++ ") reached"
          return (Right (mkResult ls))
      | otherwise = do
          hPutStrLn stderr $ "[agent] turn " ++ show (lsTurn ls + 1)
                          ++ "/" ++ show maxTurns
                          ++ " (changes: " ++ show (lsChangesApplied ls) ++ ")"
          eResp <- callOpenRouter cfg (lsMessages ls)
          case eResp of
            Left err -> do
              hPutStrLn stderr $ "[agent] API error: " ++ T.unpack err
              -- If we already applied some changes, return success anyway.
              if lsChangesApplied ls > 0
                then return (Right (mkResult ls))
                else return (Left err)
            Right AgentStopNoContent -> do
              hPutStrLn stderr "[agent] LLM stopped without content"
              return (Right (mkResult ls))
            Right (AgentFinalContent txt) -> do
              hPutStrLn stderr $ "[agent] final content (" ++ show (T.length txt) ++ " chars), session done"
              return (Right (mkResult ls))
            Right (AgentToolCalls assistantContent toolCalls) -> do
              -- Check if agent called finish
              case findFinish toolCalls of
                Just summary -> do
                  hPutStrLn stderr $ "[agent] finish called: " ++ T.unpack (T.take 100 summary)
                  return (Right (mkResult ls))
                Nothing -> do
                  let assistantMsg = mkAssistantMessage assistantContent toolCalls
                      ls' = ls { lsMessages = lsMessages ls ++ [assistantMsg] }
                  -- Execute all tool calls
                  (toolResults, ls'') <- executeToolCalls targetFp diffParser toolCalls ls'
                  let ls''' = ls'' { lsMessages = lsMessages ls'' ++ toolResults
                                   , lsTurn     = lsTurn ls'' + 1
                                   }
                  loop ls'''

    mkResult :: LoopState -> AgentResult
    mkResult ls = AgentResult
      { arChangesApplied = lsChangesApplied ls
      , arModifiedFiles  = lsModifiedFiles ls
      , arTurnsUsed      = lsTurn ls
      }

    findFinish :: [ToolCall] -> Maybe Text
    findFinish [] = Nothing
    findFinish (tc:rest)
      | tcfName (tcFunction tc) == "finish" =
          let args = parseToolArgs (tcfArguments (tcFunction tc))
          in Just (argText args "summary")
      | otherwise = findFinish rest

buildInitialUserMessage :: FilePath -> Text -> Text -> Text
buildInitialUserMessage fp src' semanticPrompt' = T.unlines
  [ "## Task"
  , ""
  , semanticPrompt'
  , ""
  , "## Starting Point: " <> T.pack fp
  , ""
  , "```haskell"
  , src'
  , "```"
  , ""
  , "Study the codebase thoroughly, then implement the changes described above."
  , "Use submit_change to apply each change live. Call finish when done."
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Tool execution
-- ─────────────────────────────────────────────────────────────────────────────

executeToolCalls
  :: FilePath -> DiffParser -> [ToolCall] -> LoopState -> IO ([Aeson.Value], LoopState)
executeToolCalls targetFp diffParser tcs ls0 = go tcs ls0 []
  where
    go [] st acc = return (reverse acc, st)
    go (tc:rest) st acc = do
      let name = tcfName (tcFunction tc)
          args = tcfArguments (tcFunction tc)
      hPutStrLn stderr $ "[agent] tool_call: " ++ T.unpack name
      (result, st') <- dispatch name args st
      hPutStrLn stderr $ "[agent] tool result: " ++ show (T.length result) ++ " chars"
      let toolMsg = Aeson.object
            [ "role"         .= ("tool" :: Text)
            , "tool_call_id" .= tcId tc
            , "content"      .= result
            ]
      go rest st' (toolMsg : acc)

    dispatch :: Text -> Text -> LoopState -> IO (Text, LoopState)
    dispatch "try_change" args st
      | lsBuildOps st >= maxBuildOps =
          return ("Error: build operation limit (" <> T.pack (show maxBuildOps) <> ") reached. Use submit_change for your best diff.", st)
      | otherwise = do
          r <- toolTryChange targetFp diffParser args
          return (r, st { lsBuildOps = lsBuildOps st + 1 })
    dispatch "submit_change" args st
      | lsBuildOps st >= maxBuildOps =
          return ("Error: build operation limit (" <> T.pack (show maxBuildOps) <> ") reached.", st)
      | otherwise = do
          (r, mFp) <- toolSubmitChange targetFp diffParser args
          let succeeded = "SUBMIT SUCCESS" `T.isPrefixOf` r
              st' = st { lsBuildOps = lsBuildOps st + 1
                       , lsChangesApplied =
                           if succeeded then lsChangesApplied st + 1
                                        else lsChangesApplied st
                       , lsModifiedFiles =
                           case mFp of
                             Just fp | succeeded -> fp : lsModifiedFiles st
                             _                   -> lsModifiedFiles st
                       }
          return (r, st')
    dispatch "finish" _args st =
      return ("Finished.", st)
    dispatch name args st = do
      r <- executeTool targetFp tc'
      return (r, st)
      where tc' = ToolCall "" (ToolCallFunction name args)

executeTool :: FilePath -> ToolCall -> IO Text
executeTool _targetFp tc = do
  let name = tcfName (tcFunction tc)
      args = tcfArguments (tcFunction tc)
  case name of
    "read_file"       -> toolReadFile args
    "list_files"      -> toolListFiles args
    "search_code"     -> toolSearchCode args
    "get_module_info" -> toolGetModuleInfo args
    "get_test_file"   -> toolGetTestFile
    "run_build"       -> toolRunBuild
    "run_tests"       -> toolRunTests
    _                 -> return $ "Error: unknown tool '" <> name <> "'"

-- ─────────────────────────────────────────────────────────────────────────────
-- Individual tool implementations
-- ─────────────────────────────────────────────────────────────────────────────

toolReadFile :: Text -> IO Text
toolReadFile argsJson = do
  let args = parseToolArgs argsJson
      path = argText args "path"
  if T.null path
    then return "Error: 'path' argument is required"
    else do
      let fp = T.unpack path
      exists <- doesFileExist fp
      if not exists
        then return $ "Error: file not found: " <> path
        else do
          content <- TIO.readFile fp `catch` \(e :: SomeException) ->
            return $ "Error reading file: " <> T.pack (show e)
          let allLines  = T.lines content
              mStart    = argInt args "start_line"
              mEnd      = argInt args "end_line"
              startLine = maybe 1 id mStart
              selected  = case (mStart, mEnd) of
                (Just s, Just e) -> take (e - s + 1) (drop (s - 1) allLines)
                (Just s, Nothing) -> take 200 (drop (s - 1) allLines)
                _                -> take 200 allLines
              numbered = zipWith (\i l -> T.pack (show i) <> "\t" <> l)
                           [startLine ..] selected
              suffix   = if length allLines > startLine + length selected - 1
                         then ["... (" <> T.pack (show (length allLines)) <> " total lines)"]
                         else []
          return (T.unlines (numbered ++ suffix))

toolListFiles :: Text -> IO Text
toolListFiles argsJson = do
  let args = parseToolArgs argsJson
      dir  = T.unpack (argText args "directory")
      ext  = argText args "extension"
  if null dir
    then return "Error: 'directory' argument is required"
    else do
      exists <- doesDirectoryExist dir
      if not exists
        then return $ "Error: directory not found: " <> T.pack dir
        else do
          files <- listDirectory dir `catch` \(e :: SomeException) ->
            return ["Error: " ++ show e]
          let filtered = if T.null ext
                then files
                else filter (\f -> takeExtension f == T.unpack ext) files
              capped = take 100 filtered
          return $ T.unlines (map T.pack capped)

toolSearchCode :: Text -> IO Text
toolSearchCode argsJson = do
  let args     = parseToolArgs argsJson
      pattern' = argText args "pattern"
      dir      = let d = argText args "directory" in if T.null d then "." else d
  if T.null pattern'
    then return "Error: 'pattern' argument is required"
    else do
      hsFiles <- findHsFiles (T.unpack dir)
      results <- searchFiles pattern' hsFiles
      let capped    = take 50 results
          formatted = map (\(fp, lineNum, line) ->
            T.pack fp <> ":" <> T.pack (show lineNum) <> ": " <> T.strip line)
            capped
          suffix = if length results > 50
            then ["\n... (" <> T.pack (show (length results)) <> " total matches)"]
            else []
      return $ T.unlines (formatted ++ suffix)

toolGetModuleInfo :: Text -> IO Text
toolGetModuleInfo argsJson = do
  let args = parseToolArgs argsJson
      path = argText args "path"
  if T.null path
    then return "Error: 'path' argument is required"
    else do
      let fp = T.unpack path
      exists <- doesFileExist fp
      if not exists
        then return $ "Error: file not found: " <> path
        else do
          content <- TIO.readFile fp `catch` \(e :: SomeException) ->
            return $ "Error: " <> T.pack (show e)
          let funcs     = extractFunctions content
              formatted = map formatFunc funcs
          return $ T.unlines $
            [ "Module: " <> path
            , "Functions: " <> T.pack (show (length funcs))
            , ""
            ] ++ formatted
  where
    formatFunc fi =
      "  " <> fiName fi
        <> maybe "" (\sig -> " " <> sig) (fiTypeSig fi)
        <> " (line " <> T.pack (show (fiLineStart fi))
        <> ", " <> T.pack (show (fiLineCount fi)) <> " lines"
        <> ", complexity " <> T.pack (show (fiComplexity fi)) <> ")"

toolGetTestFile :: IO Text
toolGetTestFile = do
  let fp = "test/Spec.hs"
  exists <- doesFileExist fp
  if not exists
    then return "Error: test/Spec.hs not found"
    else do
      content <- TIO.readFile fp `catch` \(e :: SomeException) ->
        return $ "Error: " <> T.pack (show e)
      let ls = T.lines content
          -- Return first 200 lines with line numbers
          selected = take 200 ls
          numbered = zipWith (\i l -> T.pack (show i) <> "\t" <> l) [1::Int ..] selected
          suffix = if length ls > 200
                   then ["... (" <> T.pack (show (length ls)) <> " total lines)"]
                   else []
      return (T.unlines (numbered ++ suffix))

toolRunBuild :: IO Text
toolRunBuild = do
  result <- buildPreflight
  case result of
    Right () -> return "Build succeeded."
    Left err -> return $ "Build FAILED:\n" <> err

toolRunTests :: IO Text
toolRunTests = do
  result <- testSelf
  return (formatTestResult result)

-- | Dry-run: apply diff, build+test, ALWAYS revert, return results.
toolTryChange :: FilePath -> DiffParser -> Text -> IO Text
toolTryChange defaultFp diffParser argsJson = do
  let args     = parseToolArgs argsJson
      diffText = argText args "diff"
  if T.null diffText
    then return "Error: 'diff' argument is required"
    else case diffParser diffText of
      Left err -> return $ "Error parsing diff: " <> err
      Right (mPath, hm) -> do
        let fp = maybe defaultFp id mPath
        exists <- doesFileExist fp
        if not exists
          then return $ "Error: target file not found: " <> T.pack fp
          else do
            original <- TIO.readFile fp
            case hmTransform hm original of
              Left err -> return $ "Error applying diff: " <> err
              Right mutated -> do
                result <- (do
                  TIO.writeFile fp mutated
                  buildResult <- buildPreflight
                  case buildResult of
                    Left buildErr -> return $ "BUILD FAILED:\n" <> buildErr
                    Right () -> do
                      testResult <- testSelf
                      return (formatTestResult testResult)
                  ) `finally` TIO.writeFile fp original  -- ALWAYS revert
                return result

-- | Live apply: apply diff, build+test.  KEEP if pass, REVERT if fail.
--
-- Returns @(resultText, Just filePath)@ on any attempt.  The caller inspects
-- the result text prefix to determine success.
toolSubmitChange :: FilePath -> DiffParser -> Text -> IO (Text, Maybe FilePath)
toolSubmitChange defaultFp diffParser argsJson = do
  let args     = parseToolArgs argsJson
      diffText = argText args "diff"
  if T.null diffText
    then return ("Error: 'diff' argument is required", Nothing)
    else case diffParser diffText of
      Left err -> return ("Error parsing diff: " <> err, Nothing)
      Right (mPath, hm) -> do
        let fp = maybe defaultFp id mPath
        exists <- doesFileExist fp
        if not exists
          then return ("Error: target file not found: " <> T.pack fp, Just fp)
          else do
            original <- TIO.readFile fp
            case hmTransform hm original of
              Left err -> return ("Error applying diff: " <> err, Just fp)
              Right mutated -> do
                -- Apply the change
                TIO.writeFile fp mutated
                -- Build
                buildResult <- buildPreflight
                case buildResult of
                  Left buildErr -> do
                    TIO.writeFile fp original  -- revert on build failure
                    return ("SUBMIT FAILED (build):\n" <> buildErr, Just fp)
                  Right () -> do
                    -- Test
                    testResult <- testSelf
                    case testResult of
                      CompileSuccess passed failed _lat failedNames
                        | failed == 0 -> do
                            -- SUCCESS: keep the change on disk
                            hPutStrLn stderr $ "[agent] submit_change SUCCESS for " ++ fp
                            return ( "SUBMIT SUCCESS: All " <> T.pack (show passed)
                                   <> " tests passed. Change is now live on disk at "
                                   <> T.pack fp <> "."
                                   , Just fp)
                        | otherwise -> do
                            TIO.writeFile fp original  -- revert on test failure
                            let fSection = if null failedNames then ""
                                  else "\nFailed tests:\n" <> T.unlines (map ("  - " <>) failedNames)
                            return ( "SUBMIT FAILED (tests): "
                                   <> T.pack (show passed) <> " passed, "
                                   <> T.pack (show failed) <> " failed." <> fSection
                                   , Just fp)
                      CompileFailure errs phase -> do
                        TIO.writeFile fp original  -- revert on failure
                        return ( "SUBMIT FAILED (" <> T.pack (show phase) <> "):\n"
                               <> T.take 500 errs
                               , Just fp)

-- ─────────────────────────────────────────────────────────────────────────────
-- HTTP helpers
-- ─────────────────────────────────────────────────────────────────────────────

callOpenRouter :: AgentConfig -> [Aeson.Value] -> IO (Either Text AgentResponse)
callOpenRouter cfg messages = do
  let url     = T.unpack (acBaseUrl cfg) <> "/chat/completions"
      bodyLBS = Aeson.encode $ Aeson.object
        [ "model"       .= acModel cfg
        , "temperature" .= (0.4 :: Double)
        , "max_tokens"  .= (4096 :: Int)
        , "tools"       .= agentTools
        , "messages"    .= messages
        ]
  eReq <- try (HTTP.parseRequest url) :: IO (Either SomeException HTTP.Request)
  case eReq of
    Left err -> return (Left ("agent: bad URL: " <> T.pack (show err)))
    Right req0 -> do
      let req = HTTP.setRequestMethod "POST"
              $ HTTP.addRequestHeader "Content-Type" "application/json"
              $ HTTP.addRequestHeader "Authorization"
                  ("Bearer " <> BS8.pack (T.unpack (acApiKey cfg)))
              $ HTTP.setRequestBodyLBS bodyLBS req0
      eResp <- try (HTTP.httpLBS req)
                 :: IO (Either SomeException (HTTP.Response LBS.ByteString))
      case eResp of
        Left err   -> return (Left ("agent: HTTP error: " <> T.pack (show err)))
        Right resp -> return (parseAgentResponse (HTTP.getResponseBody resp))

-- | Build the assistant message with tool calls for the conversation history.
mkAssistantMessage :: Text -> [ToolCall] -> Aeson.Value
mkAssistantMessage content tcs = Aeson.object $
  [ "role" .= ("assistant" :: Text) ] ++
  (if T.null content then [] else ["content" .= content]) ++
  [ "tool_calls" .= map tcToJson tcs ]
  where
    tcToJson tc = Aeson.object
      [ "id"       .= tcId tc
      , "type"     .= ("function" :: Text)
      , "function" .= Aeson.object
        [ "name"      .= tcfName (tcFunction tc)
        , "arguments" .= tcfArguments (tcFunction tc)
        ]
      ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Formatting helpers
-- ─────────────────────────────────────────────────────────────────────────────

formatTestResult :: CompileResult -> Text
formatTestResult (CompileSuccess passed failed _lat failedNames) =
  let total  = passed + failed
      status = if failed == 0 then "ALL PASSED" else "SOME FAILED"
      fSection = if null failedNames then ""
        else "\n\nFailed tests:\n" <> T.unlines (map ("  - " <>) failedNames)
  in T.unlines
       [ "Tests: " <> status
       , "Passed: " <> T.pack (show passed) <> "/" <> T.pack (show total)
       , "Failed: " <> T.pack (show failed)
       ] <> fSection
formatTestResult (CompileFailure errs phase) =
  "Tests FAILED (" <> T.pack (show phase) <> "):\n" <> T.take 500 errs

-- ─────────────────────────────────────────────────────────────────────────────
-- File searching utilities
-- ─────────────────────────────────────────────────────────────────────────────

-- | Recursively find all @.hs@ files under a directory.
findHsFiles :: FilePath -> IO [FilePath]
findHsFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists then return [] else do
    entries <- listDirectory dir `catch` \(_ :: SomeException) -> return []
    fmap concat $ mapM (\entry -> do
      let full = dir </> entry
      isDir <- doesDirectoryExist full
      if isDir && not (shouldSkip entry)
        then findHsFiles full
        else if takeExtension entry == ".hs"
          then return [full]
          else return []
      ) entries
  where
    shouldSkip name = name `elem`
      ["dist-newstyle", ".git", "gt", ".beads", ".claude", "node_modules"]

-- | Search files for a text pattern, returning @(filePath, lineNum, lineText)@.
searchFiles :: Text -> [FilePath] -> IO [(FilePath, Int, Text)]
searchFiles pattern' files = fmap concat $ mapM searchOne files
  where
    searchOne fp = do
      content <- TIO.readFile fp `catch` \(_ :: SomeException) -> return ""
      let ls = zip [1::Int ..] (T.lines content)
          matches = [(fp, n, l) | (n, l) <- ls, pattern' `T.isInfixOf` l]
      return matches

#else

-- ─────────────────────────────────────────────────────────────────────────────
-- Stubs (with-oracle flag absent)
-- ─────────────────────────────────────────────────────────────────────────────

-- | Agentic oracle is unavailable without the with-oracle flag.
runAgentOracle
  :: AgentConfig
  -> (Text -> Either Text (Maybe FilePath, HsMutation))
  -> FilePath -> Text -> Text
  -> IO (Either Text AgentResult)
runAgentOracle _cfg _parser _fp _src _prompt =
  return (Left "DGM.AgentOracle: build with -f+with-oracle to enable agentic oracle")

-- | Tool definitions (empty without oracle flag).
agentTools :: [Aeson.Value]
agentTools = []

#endif

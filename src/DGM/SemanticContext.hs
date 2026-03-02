-- | DGM.SemanticContext — Deep module analysis for engineer-grade Oracle prompts.
--
-- Extracts structured information from Haskell source files: type signatures,
-- function boundaries, complexity metrics, and test coverage mappings.  This
-- semantic context enables the Oracle to reason about /what/ to improve rather
-- than blindly proposing lint-level changes.
module DGM.SemanticContext
  ( -- * Types
    FunctionInfo(..)
  , TestMapping(..)
  , SemanticContext(..)
    -- * Extraction (pure, testable)
  , extractFunctions
    -- * Test coverage
  , buildTestCoverage
    -- * Full context builder
  , buildSemanticContext
  ) where

import Data.Char (isAlphaNum, isLower, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (SomeException, catch)

import DGM.ModGraph (ModuleGraph(..), extractExports)
import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Structured information about a single top-level function.
data FunctionInfo = FunctionInfo
  { fiName       :: !Text           -- ^ e.g. @\"buildModuleGraph\"@
  , fiTypeSig    :: Maybe Text      -- ^ e.g. @\":: [FilePath] -> IO ModuleGraph\"@
  , fiLineStart  :: !Int            -- ^ 1-based line number of definition start
  , fiLineCount  :: !Int            -- ^ number of lines in the function body
  , fiCalls      :: [Text]          -- ^ other top-level names referenced in body
  , fiComplexity :: !Int            -- ^ case branches + guards + wheres + nested lambdas
  } deriving (Show, Eq)

-- | Mapping from a test case to the function it tests.
data TestMapping = TestMapping
  { tmTestName   :: !Text           -- ^ e.g. @\"buildModuleGraph: core DGM modules...\"@
  , tmTestedFunc :: !Text           -- ^ e.g. @\"buildModuleGraph\"@
  , tmTestFile   :: !FilePath
  } deriving (Show, Eq)

-- | Full semantic context for a module, combining static analysis and history.
--
-- Note: GHC warnings are NOT stored here to avoid a circular module dependency
-- with 'DGM.OracleContext'.  The caller passes warnings separately.
data SemanticContext = SemanticContext
  { scFilePath        :: !FilePath
  , scModuleName      :: !Text
  , scFunctions       :: [FunctionInfo]
  , scTestCoverage    :: [TestMapping]
  , scUntestedExports :: [Text]            -- ^ exported names with no test coverage
  , scUsedBy          :: [(Text, [Text])]  -- ^ (moduleName, [importingModules])
  , scRecentFailures  :: [(Text, Text)]    -- ^ (mutation desc, failure reason)
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Function extraction (pure)
-- ---------------------------------------------------------------------------

-- | Extract top-level function information from Haskell source text.
--
-- Scans for type signature lines (@name :: ...@) and definition lines
-- (@name arg1 ... = ...@).  Function body extent is determined by
-- indentation: the body continues until a line at column 0 that starts a
-- new top-level definition.
extractFunctions :: Text -> [FunctionInfo]
extractFunctions src =
  let ls = zip [1 :: Int ..] (T.lines src)
      sigs = collectTypeSigs ls
      defs = collectDefs ls
      topNames = map fst defs
  in  map (buildInfo sigs topNames ls) defs

-- | Collect type signatures: @name :: Type@
collectTypeSigs :: [(Int, Text)] -> [(Text, Text)]
collectTypeSigs [] = []
collectTypeSigs ((_lineNo, line) : rest) =
  case parseTypeSig line of
    Just (name, sig) ->
      let contLines = takeWhile isSigCont rest
          fullSig = sig <> T.concat [" " <> T.strip (snd l) | l <- contLines]
      in (name, fullSig) : collectTypeSigs (drop (length contLines) rest)
    Nothing -> collectTypeSigs rest
  where
    isSigCont (_, l) =
      let s = T.strip l
      in not (T.null s)
         && not (isTopLevel l)
         && not (T.isPrefixOf "--" s)

-- | Parse @name :: rest@ from a line.
parseTypeSig :: Text -> Maybe (Text, Text)
parseTypeSig line =
  let s = T.stripStart line
  in if T.isPrefixOf "--" s || T.null s || T.isPrefixOf "{-" s
     then Nothing
     else case T.breakOn " :: " s of
            (before, after)
              | not (T.null after) ->
                let name = T.strip before
                    sig  = T.strip (T.drop 4 after)  -- drop " :: "
                in if isValidFuncName name
                   then Just (name, ":: " <> sig)
                   else Nothing
            _ -> Nothing

-- | Collect function definitions: @name args = ...@
collectDefs :: [(Int, Text)] -> [(Text, (Int, Int))]
collectDefs [] = []
collectDefs ((lineNo, line) : rest) =
  case parseDefStart line of
    Just name ->
      let bodyLines = takeWhile (\(_, l) -> not (isTopLevel l) || T.null (T.strip l)) rest
          lineCount = 1 + length bodyLines
      in (name, (lineNo, lineCount)) : collectDefs (drop (length bodyLines) rest)
    Nothing -> collectDefs rest

-- | Parse @name args = ...@ definition start.
parseDefStart :: Text -> Maybe Text
parseDefStart line
  | not (isTopLevel line) = Nothing
  | otherwise =
      let s = T.strip line
      in if any (`T.isPrefixOf` s)
              [ "--", "module ", "import ", "type ", "data ", "newtype "
              , "class ", "instance ", "infixl ", "infixr ", "infix "
              , "{-", "#" ]
         then Nothing
         else case T.words s of
                (name : rest)
                  | isValidFuncName name
                  , hasEqualsSign rest
                  -> Just name
                _ -> Nothing
  where
    hasEqualsSign ws = any (\w -> w == "=" || w == "|" || "=" `T.isSuffixOf` w) ws

isTopLevel :: Text -> Bool
isTopLevel line = not (T.null line) && not (isSpace (T.head line))

isValidFuncName :: Text -> Bool
isValidFuncName t =
  not (T.null t) && isLower (T.head t)
  && T.all (\c -> isAlphaNum c || c == '_' || c == '\'') t

buildInfo :: [(Text, Text)] -> [Text] -> [(Int, Text)] -> (Text, (Int, Int)) -> FunctionInfo
buildInfo sigs topNames allLines (name, (lineStart, lineCount)) =
  let mSig = lookup name sigs
      bodyLines = map snd $ take lineCount $ drop (lineStart - 1) allLines
      bodyText = T.unlines bodyLines
      complexity = countComplexity bodyText
      calls = findCalls topNames name bodyText
  in FunctionInfo
       { fiName       = name
       , fiTypeSig    = mSig
       , fiLineStart  = lineStart
       , fiLineCount  = lineCount
       , fiCalls      = calls
       , fiComplexity = complexity
       }

-- | Count complexity: case branches, guards, wheres, nested lambdas.
countComplexity :: Text -> Int
countComplexity bodyText =
  let ls = T.lines bodyText
      caseCount   = length [l | l <- ls, " case " `T.isInfixOf` l
                                       || T.isPrefixOf "case " (T.strip l)]
      guardCount  = length [l | l <- ls, let s = T.stripStart l
                                         in T.isPrefixOf "| " s
                                            && not (T.isPrefixOf "| otherwise" s)]
      whereCount  = length [l | l <- ls, T.strip l == "where"
                                       || T.isSuffixOf " where" (T.strip l)]
      lambdaCount = length [l | l <- ls, "\\ " `T.isInfixOf` l
                                       || "\\x" `T.isInfixOf` l
                                       || T.isPrefixOf "\\" (T.strip l)]
  in  max 1 (caseCount + guardCount + whereCount + lambdaCount)

-- | Find calls to other top-level functions within a body.
findCalls :: [Text] -> Text -> Text -> [Text]
findCalls topNames selfName bodyText =
  [ name | name <- topNames
         , name /= selfName
         , wholeWordIn name bodyText
  ]
  where
    wholeWordIn needle hay =
      case T.breakOn needle hay of
        (_, rest) | T.null rest -> False
        (before, rest) ->
          let afterNeedle = T.drop (T.length needle) rest
              beforeOk = T.null before
                      || not (isIdentChar (T.last before))
              afterOk  = T.null afterNeedle
                      || not (isIdentChar (T.head afterNeedle))
          in  (beforeOk && afterOk) || wholeWordIn needle afterNeedle

    isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- ---------------------------------------------------------------------------
-- Test coverage
-- ---------------------------------------------------------------------------

-- | Build test coverage mappings by scanning the test file.
--
-- Reads @test/Spec.hs@ and matches test descriptions against function names.
buildTestCoverage :: [FunctionInfo] -> IO [TestMapping]
buildTestCoverage funcs = do
  mTestSrc <- readTestFile
  case mTestSrc of
    Nothing  -> return []
    Just testSrc ->
      let testNames = parseTestNames testSrc
          funcNames = map fiName funcs
      in  return [ TestMapping tName fName "test/Spec.hs"
                 | (tName, _) <- testNames
                 , fName <- funcNames
                 , fName `T.isInfixOf` tName
                 ]
  where
    readTestFile :: IO (Maybe Text)
    readTestFile =
      (Just <$> TIO.readFile "test/Spec.hs")
        `catch` (\(_ :: SomeException) -> return Nothing)

-- | Parse test case/property names from test source.
parseTestNames :: Text -> [(Text, Int)]
parseTestNames src =
  concatMap parseLine (zip [1 :: Int ..] (T.lines src))
  where
    parseLine (lineNo, line) =
      let s = T.strip line
      in  concatMap (\prefix -> maybeToList (extractQuotedAfter prefix s lineNo))
            ["testCase", "testProperty"]

    extractQuotedAfter :: Text -> Text -> Int -> Maybe (Text, Int)
    extractQuotedAfter prefix s lineNo
      | prefix `T.isInfixOf` s =
          let afterPrefix = snd (T.breakOn prefix s)
          in case T.breakOn "\"" afterPrefix of
               (_, rest)
                 | not (T.null rest) ->
                   let inner = T.takeWhile (/= '"') (T.drop 1 rest)
                   in  if T.null inner then Nothing else Just (inner, lineNo)
               _ -> Nothing
      | otherwise = Nothing

    maybeToList Nothing  = []
    maybeToList (Just x) = [x]

-- ---------------------------------------------------------------------------
-- Full context builder
-- ---------------------------------------------------------------------------

-- | Build a complete 'SemanticContext' for a module file.
buildSemanticContext
  :: ModuleGraph
  -> [(Text, Text)]  -- ^ Recent failures: @(mutationDesc, failureReason)@
  -> FilePath
  -> Text            -- ^ Module source text
  -> IO SemanticContext
buildSemanticContext mg recentFailures fp src = do
  let funcs = extractFunctions src
  testMappings <- buildTestCoverage funcs

  let exports = extractExports src
      testedNames = map tmTestedFunc testMappings
      untestedExports = [ e | e <- exports, e `notElem` testedNames ]

      -- Module name lookup
      mModName = case [ nm | (nm, path) <- Map.toList (mgModules mg)
                           , path == fp ] of
                   (n:_) -> Just n
                   []    -> Nothing
      modName = maybe (T.pack fp) id mModName

      -- Cross-module usage
      usedByList = case mModName of
                     Nothing -> []
                     Just nm -> case Map.lookup nm (mgUsedBy mg) of
                                  Nothing   -> []
                                  Just deps -> [(nm, deps)]

  return SemanticContext
    { scFilePath        = fp
    , scModuleName      = modName
    , scFunctions       = funcs
    , scTestCoverage    = testMappings
    , scUntestedExports = untestedExports
    , scUsedBy          = usedByList
    , scRecentFailures  = recentFailures
    }

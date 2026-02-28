-- | DGM.ModGraph — Module dependency graph and cross-module mutation safety.
--
-- Implements SELFMOD.md Phase F: builds a live module dependency graph from
-- parsed import/export headers, enabling cross-module mutation ranking and
-- export-removal rejection.
--
-- The graph is computed purely from source text (no GHC API required), so it
-- works regardless of whether the @with-exactprint@ cabal flag is set.
module DGM.ModGraph
  ( -- * Types
    ModuleGraph(..)
  , emptyModuleGraph
    -- * Construction
  , buildModuleGraph
    -- * Queries
  , numDependents
  , moduleImpact
    -- * Export safety
  , extractExports
  , removesExportedName
  , removesExportedNameText
  ) where

import Control.Exception (SomeException, catch)
import Data.Char (isAlphaNum, isAlpha)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath (takeBaseName, splitDirectories, normalise)

import DGM.HsAST (HsNode(..), HsMutation(..), HsModule, applyHsMutation, printHsModule)
import DGM.Types (ModuleName)

-- ─────────────────────────────────────────────────────────────────────────────
-- Types
-- ─────────────────────────────────────────────────────────────────────────────

-- | Live module dependency graph computed from source file headers.
--
-- All four maps are keyed by 'ModuleName'.
data ModuleGraph = ModuleGraph
  { mgModules :: Map ModuleName FilePath
    -- ^ Mapping from module name to source file path.
  , mgImports :: Map ModuleName [ModuleName]
    -- ^ Direct imports: @mgImports ! M@ = modules imported by M.
  , mgExports :: Map ModuleName [HsNode]
    -- ^ Exported names: @mgExports ! M@ = nodes exported by M.
  , mgUsedBy  :: Map ModuleName [ModuleName]
    -- ^ Reverse dependency: @mgUsedBy ! M@ = modules that import M.
  } deriving (Show)

-- | An empty module graph.
emptyModuleGraph :: ModuleGraph
emptyModuleGraph = ModuleGraph Map.empty Map.empty Map.empty Map.empty

-- ─────────────────────────────────────────────────────────────────────────────
-- Construction
-- ─────────────────────────────────────────────────────────────────────────────

-- | Build a module dependency graph from a list of Haskell source files.
--
-- Reads each file and extracts:
--   * The module name (from the @module M@ declaration, or derived from path)
--   * Imported module names (from all @import@ lines)
--   * Exported names (from the explicit export list, if present)
--
-- The 'mgUsedBy' reverse-dependency map is derived by inverting 'mgImports'.
-- Unreadable files are silently skipped.
buildModuleGraph :: [FilePath] -> IO ModuleGraph
buildModuleGraph fps = do
  entries <- mapM parseOneFile fps
  let validEntries = [(nm, fp, imps, exps) | Just (nm, fp, imps, exps) <- entries]
      modules = Map.fromList [(nm, fp)   | (nm, fp, _, _)    <- validEntries]
      imports = Map.fromList [(nm, imps) | (nm, _,  imps, _) <- validEntries]
      exports = Map.fromList [(nm, exps) | (nm, _,  _, exps) <- validEntries]
      usedBy  = buildUsedBy imports
  return ModuleGraph
    { mgModules = modules
    , mgImports = imports
    , mgExports = exports
    , mgUsedBy  = usedBy
    }
  where
    parseOneFile :: FilePath -> IO (Maybe (ModuleName, FilePath, [ModuleName], [HsNode]))
    parseOneFile fp = do
      msrc <- readFileSafe fp
      case msrc of
        Nothing  -> return Nothing
        Just src ->
          let nm       = extractModuleName src fp
              imps     = extractModuleImports src
              expNames = extractExports src
              expNodes = map (\n -> HsNode { hnText = n, hnKind = "export", hnLine = 0 }) expNames
          in  return (Just (nm, fp, imps, expNodes))

    readFileSafe :: FilePath -> IO (Maybe Text)
    readFileSafe fp =
      (Just <$> TIO.readFile fp)
        `catch` (\(_ :: SomeException) -> return Nothing)

    -- | Invert the imports map to produce the usedBy map.
    buildUsedBy :: Map ModuleName [ModuleName] -> Map ModuleName [ModuleName]
    buildUsedBy imps = foldl' addRev Map.empty (Map.toList imps)
      where
        addRev acc (importer, importedList) =
          foldl' (\m dep -> Map.insertWith (++) dep [importer] m) acc importedList

-- ─────────────────────────────────────────────────────────────────────────────
-- Header parsing
-- ─────────────────────────────────────────────────────────────────────────────

-- | Extract the module name from source text.
--
-- Scans for the first @module M@ line and extracts @M@.  Falls back to
-- deriving the module name from the file path (dot-joined directory components
-- after @src/@, with the @.hs@ extension dropped) if no declaration is found.
extractModuleName :: Text -> FilePath -> ModuleName
extractModuleName src fp =
  case concatMap parseModLine (T.lines src) of
    (name:_) -> name
    []       -> moduleNameFromPath fp
  where
    parseModLine line =
      let s = T.stripStart line
      in if "module " `T.isPrefixOf` s
         then
           let ws = T.words s
           in case ws of
                (_:nm:_) -> [T.takeWhile (\c -> isAlphaNum c || c == '.') nm]
                _        -> []
         else []

    moduleNameFromPath :: FilePath -> ModuleName
    moduleNameFromPath path =
      let comps  = splitDirectories (normalise path)
          -- Strip leading "./" and "src/" components (as Strings).
          clean  = dropWhile (\c -> c `elem` ([".", "src"] :: [String])) comps
          -- Remove ".hs" extension from the final component.
          noExt  = case reverse clean of
                     []     -> clean
                     (x:xs) -> reverse (takeBaseName x : xs)
      in T.intercalate "." (map T.pack noExt)

-- | Extract imported module names from source text.
--
-- Handles plain, qualified, safe, and @import qualified@ orderings.
-- Returns the bare module name (e.g. @\"DGM.Types\"@, @\"Data.Map.Strict\"@).
extractModuleImports :: Text -> [ModuleName]
extractModuleImports src = concatMap parseImportLine (T.lines src)
  where
    parseImportLine line =
      let s = T.stripStart line
      in if "import " `T.isPrefixOf` s
         then
           let ws   = T.words s
               -- Drop the leading "import" and any "qualified"/"safe" keywords.
               rest = dropWhile (`elem` (["import", "qualified", "safe"] :: [Text])) ws
           in case rest of
                (nm:_) -> [T.takeWhile (\c -> isAlphaNum c || c == '.') nm]
                []     -> []
         else []

-- | Extract exported names from the module's explicit export list.
--
-- Returns an empty list when:
--   * There is no @module M (...)@ export list (wildcard / re-export-all).
--   * The export list is empty.
--
-- Names are the bare identifiers (e.g. @\"SafetyLevel\"@, @\"buildModuleGraph\"@)
-- stripped of any @(..)@ or @(Constructor)@ suffixes.
extractExports :: Text -> [Text]
extractExports src =
  let ls          = T.lines src
      afterModule = dropWhile (\l -> not ("module " `T.isPrefixOf` T.stripStart l)) ls
  in case afterModule of
       [] -> []
       _  ->
         let headerBlock = takeUntilWhere afterModule
             combined    = T.unlines headerBlock
         in if "(" `T.isInfixOf` combined
            then parseExportNames headerBlock
            else []
  where
    -- Collect header lines up to and including the closing ") where".
    takeUntilWhere :: [Text] -> [Text]
    takeUntilWhere []     = []
    takeUntilWhere (l:ls)
      | isWhereLine l    = [l]
      | otherwise        = l : takeUntilWhere ls

    isWhereLine :: Text -> Bool
    isWhereLine l =
      let s = T.strip l
      in s == "where"
         || ") where" `T.isSuffixOf` s
         || "where " `T.isPrefixOf` s

    -- Extract one identifier per export-list line.
    parseExportNames :: [Text] -> [Text]
    parseExportNames = concatMap extractFromLine
      where
        extractFromLine line =
          let s = T.strip line
          in if "--" `T.isPrefixOf` s || T.null s
             then []
             else
               let -- Remove leading punctuation / whitespace typical of export lists.
                   clean = T.dropWhile (`elem` ("(),\t " :: String)) s
                   -- Take the first identifier (letters, digits, _, ').
                   name  = T.takeWhile (\c -> isAlphaNum c || c `elem` ("_'" :: String)) clean
               in if T.null name || not (isAlpha (T.head name))
                  then []
                  else [name]

-- ─────────────────────────────────────────────────────────────────────────────
-- Queries
-- ─────────────────────────────────────────────────────────────────────────────

-- | Number of modules that directly import the module at @fp@.
numDependents :: ModuleGraph -> FilePath -> Int
numDependents mg fp =
  case lookupByPath fp mg of
    Nothing   -> 0
    Just name -> maybe 0 length (Map.lookup name (mgUsedBy mg))

-- | Look up a module name by its source file path.
lookupByPath :: FilePath -> ModuleGraph -> Maybe ModuleName
lookupByPath fp mg =
  case [nm | (nm, path) <- Map.toList (mgModules mg), path == fp] of
    (n:_) -> Just n
    []    -> Nothing

-- | Module impact score for use in 'DGM.SelfMod.rankMutations'.
--
-- Returns the number of modules that depend on @fp@'s exported interface IF
-- applying @mut@ to @m@ changes the export list; 0 otherwise.
--
-- Concretely:
--   * If the mutated module has the same export list as the original → 0 impact.
--   * If the export list changes → @'numDependents' mg fp@.
moduleImpact :: ModuleGraph -> FilePath -> HsMutation -> HsModule -> Int
moduleImpact mg fp mut m =
  case applyHsMutation mut m of
    Left _   -> 0  -- inapplicable; no impact
    Right m' ->
      let origExps = Set.fromList (extractExports (printHsModule m))
          mutExps  = Set.fromList (extractExports (printHsModule m'))
      in if origExps /= mutExps
         then numDependents mg fp
         else 0

-- ─────────────────────────────────────────────────────────────────────────────
-- Export safety
-- ─────────────────────────────────────────────────────────────────────────────

-- | True if applying @mut@ to @m@ removes at least one currently-exported name.
--
-- Used by 'DGM.Cycle.runSelfModCycle' to reject export-removing mutations
-- pre-write.
removesExportedName :: HsMutation -> HsModule -> Bool
removesExportedName mut m =
  removesExportedNameText mut (printHsModule m)

-- | Like 'removesExportedName' but operates on raw source text.
--
-- Applies @mut@'s 'hmTransform' directly to @origSrc@ without going through
-- the full 'applyHsMutation' pipeline.  Useful for testing or for callers
-- that have source text but no parsed 'HsModule'.
removesExportedNameText :: HsMutation -> Text -> Bool
removesExportedNameText mut origSrc =
  case hmTransform mut origSrc of
    Left _      -> False  -- inapplicable; cannot remove anything
    Right mutSrc ->
      let origExps = Set.fromList (extractExports origSrc)
          mutExps  = Set.fromList (extractExports mutSrc)
      in not (Set.null (Set.difference origExps mutExps))

{-# LANGUAGE OverloadedStrings #-}
module Parser.ModuleResolver where

import Parser.ParserIO
import Parser.Common
import Parser.Identifier
import Syntax
import GetOpt (Options(..), ModulePath(..))
import Utility
import Data.Maybe (isJust)
import Data.List (find)
import Control.Monad (when)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec as P
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath.Posix ( takeFileName
                             , dropExtension
                             , takeDirectory
                             , (<.>)
                             , (</>)
                             )

-- Parser of files. Given the file path it parses it into a module using given
-- module parser. The module parser is only passed here to achieve Dependency
-- Inversion and a cyclic module dependency.
parseFile :: FilePath 
          -> (FilePath -> ParserIO (Module ())) 
          -> ParserIO (Module ())
parseFile path moduleParser = do
    assertNoCycle path
    visited <- stateVisited <$> getState
    case Map.lookup path visited of
        Nothing -> do
            file   <- liftIO $ T.readFile path
            clearLocalParserState
            result <- liftState $ P.runParserT (moduleParser path) path file
            -- TODO: maybe handle errors with 'region'
            mod   <- handleError result
            state <- getState
            let newState = state { stateVisited = Map.insert path mod visited }
            putState newState
            return mod
        Just mod ->
            return mod
  where
    -- Handler for errors from submodules
    handleError :: Either Errors (Module ()) -> ParserIO (Module ())
    handleError (Right mod) = return mod
    handleError (Left err) = liftIO $ 
        putStrLn (P.errorBundlePretty err) >> exitFailure
    -- Function for detection of import cycles, which are forbidden.
    assertNoCycle :: FilePath -> ParserIO ()
    assertNoCycle path = do
        pathStack <- map snd . stateModuleStack <$> getState
        when (path `elem` pathStack) $
            -- TODO: ^ handle it better and find the actual module name
            fail $ "detected cyclic dependency: file " 
                ++ show path 
                ++ " was previously included in a module it depends on"

-- Find the path of a given module. The module name is divided into the
-- prefix (e.g. Data\Map) and the file name (e.g Strict). It may fail.
findModulePath :: FilePath -> [ModName] -> ModName -> ParserIO FilePath
findModulePath current prefix name = do
    externalModules <- filter matching . optModules <$> getopt
    let choices = absPath : (appendModName . optModPath <$> externalModules)
    path <- findM (liftIO . doesFileExist) choices
    case path of
        Nothing   -> fail $ "cannot find module " ++ show (unwrapName name)
        Just path -> return path
  where
    -- Absolute path to a file created from the relative path from the
    -- currently parsed file.
    absPath = current' </> relativePath
    -- Path to the currently parsed file.
    current' = takeDirectory current
    -- Path to the module file relative to current path.
    relativePath = foldPath prefix name
    -- Join the prefix and module name into a single FilePath.
    foldPath :: [ModName] -> ModName -> FilePath
    foldPath prefix (ModName name) = 
        foldl joinPath "" prefix </> (T.unpack name) <.> fileExtension
      where
        joinPath path (ModName dir) = path </> (T.unpack dir)
    -- Checks whether the first part of the module name fits some
    -- module path found in the command line arguments.
    matching :: ModulePath -> Bool
    matching (ModulePath "\\" _) = True
    matching mod
        | null prefix            = optModName mod == unwrapName name
        | otherwise              = optModName mod == unwrapName (head prefix)
    unwrapName (ModName name) = name
    appendModName :: FilePath -> FilePath
    appendModName path = path </> T.unpack (unwrapName name) <.> fileExtension

-- Given the import information locate and parse a submodule.
importFile :: (FilePath -> ParserIO (Module ())) 
           -> Located RawImport
           -> ParserIO (Import ())
importFile moduleParser (Located position (id, alias, identifiers)) = do
    let (ModuleId prefix mod) = id
    basePath <- getCurrentPath
    path     <- findModulePath basePath prefix mod
    mod      <- parseFile path moduleParser
    return $ Import mod id alias position identifiers

-- Brings operators from a module into the current scope.
importOperators :: Import a -> ParserIO ()
importOperators importedModule = do
    let hasAlias    = isJust . importAlias $ importedModule
        path        = modulePath . importMod $ importedModule
        identifiers = importIds importedModule
        pos         = importLoc importedModule
    visible  <- (concat . Map.lookup path . stateExportedOps) <$> getState
    addLocalOperators pos $ processImports identifiers visible hasAlias

-- Given the path and list of imports figure out what operators need to
-- be imported into the current operator table. If the import has an alias
-- then the operators have to be explicitly imported to be included.
processImports :: (Maybe [Located ImportedValue]) 
                -> [(CustomOperator, Priority, Fixity)]
                -> Bool
                -> [(CustomOperator, Priority, Fixity)]
processImports Nothing visible True = []
processImports Nothing visible False = visible
processImports (Just imports) visible _ =
    filter (\(op,_,_) -> unwrapOperator op `elem` strippedImports) visible
    where
    strippedImports = concat $ strip . unlocated <$> imports

-- Function which adds operators from an imported module and puts
-- them in the local operator table.
addLocalOperators :: Position 
                    ->[(CustomOperator, Priority, Fixity)] 
                    -> ParserIO ()
addLocalOperators pos = mapM_ (flip defineOperator pos)

-- Function for detection of invalid module names. Module names should 
-- match the file names.
validateModuleName :: FilePath -> ModName -> ParserIO ()
validateModuleName path name@(ModName name')
    | pathName path /= name' = fail . T.unpack $ 
        "module name \"" <> name' <> "\" doesn't match the file name"
    | otherwise = return ()
    where
    pathName = T.pack . dropExtension . takeFileName

-- Convert a ImportedValue wrapper into Text identifier(s).
strip :: ImportedValue -> [T.Text]
strip (ImportedIdentifier (Identifier id)) = [id]
strip (ImportedType _ Nothing) = [] -- TODO: import everything
strip (ImportedType _ (Just constructors)) = 
    map (\(ConstructorName name) -> name) constructors    

-- Extracts the list of constructors which should be imported based on the
-- given import statement.
getImplicitConstructors :: ImportedValue -> ParserIO [T.Text]
getImplicitConstructors = undefined -- TODO: implement

-- Add the constructor information to the global table. Only the exported
-- constructors will be visible.
exportConstructors :: ParserIO ()
exportConstructors = undefined -- TODO: implement

-- Adds all locally defined operators that are supposed to be exported to
-- the global operator export table.
exportOperators :: Maybe [Located ImportedValue] -> ParserIO ()
exportOperators Nothing = do 
    locals <- stateLocalOps <$> getState
    let localsRaw = map unwrapOperator locals
    exportOperatorsAux localsRaw
exportOperators (Just exports) = 
    exportOperatorsAux $ exports >>= (strip . unlocated)

-- Helper function that exports operators stripped from all
-- of the wrappers.
exportOperatorsAux :: [T.Text] -> ParserIO ()
exportOperatorsAux exports = do
    state <- getState
    let opTable   = stateCurrentOps state
        opList    = foldr (++) [] opTable
        globalOps = stateExportedOps state
        exports'  = filter (isOp opList) exports
    insertOpExports opTable opList exports' globalOps []
  where
    -- Actually inserts local operator definitions into the parser state.
    insertOpExports :: LocalOperators
                    -> [CustomOperator]
                    -> [T.Text]
                    -> GlobalOperators 
                    -> [(CustomOperator, Priority, Fixity)]
                    -> ParserIO ()
    insertOpExports _ _ [] globals locals = do
        path <- getCurrentPath
        let newGlobals = Map.insert path locals globals
        state <- getState
        putState $ state { stateExportedOps = newGlobals }
    insertOpExports opInfo opList (e:es) globals locals =
        case findOpInfo e opInfo of
            Just info -> 
                insertOpExports opInfo opList es globals (info : locals)
            Nothing -> fail $ "Undefined exported operator " ++ show e

-- Local operator table has fixity and priority as keys for parsing
-- performance. This function inverts the map to find the fixity
-- and priority for a given operator. It is assumed, that the operator
-- is defined. It isn't cheap but it's performed only once per file.
findOpInfo :: T.Text -> LocalOperators -> Maybe (CustomOperator, Priority, Fixity)
findOpInfo op operators = do
    opGroup <- find (elem op . map unwrapOperator . snd) operatorsAsList
    wrappedOp <- find ((== op) . unwrapOperator) (snd opGroup)
    let (priority, fixity) = fst opGroup
    return (wrappedOp, priority, fixity)
    where 
    operatorsAsList = Map.toList operators

-- Process a list of imports by adding proper operators from the global scope
-- to the local scope.
importOperatorsFromImports :: [Import ()] -> ParserIO ()
importOperatorsFromImports = mapM_ importOperators

-- Add exported local state like the operators or type constructors 
-- to the global state and clear the state
finalizeModule :: Maybe [Located ImportedValue] -> ParserIO ()
finalizeModule exports = do
    exportConstructors
    exportOperators exports -- at the end we update the operator table
    clearLocalParserState


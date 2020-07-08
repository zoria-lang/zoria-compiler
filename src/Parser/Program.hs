{-# LANGUAGE OverloadedStrings #-}
module Parser.Program (program) where

import Parser.ParserIO
import Parser.Common
import Parser.Identifier
import Parser.Type
import Parser.Definition
import Syntax
import GetOpt (Options(..), ModulePath(..))
import Utility (Position, findM)
import Data.List (find)
import Control.Monad (when)
import System.Exit (exitFailure)
import Data.Maybe (isJust, catMaybes)
import Control.Applicative ((<|>))
import Text.Megaparsec ((<?>))
import System.Directory (doesFileExist)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P
import qualified Data.Map.Strict as Map
import System.FilePath.Posix (takeFileName
                             , dropExtension
                             , takeDirectory
                             , (<.>)
                             , (</>)
                             )

-- Parser of programs. It is supposed to be used once.
program :: ParserIO (Program ())
program = do
    -- TODO: import prelude
    rootFile <- head . optInputs <$> getopt
    Program <$> file rootFile

-- Parser of files. Given the file path it parses it into a module.
file :: FilePath -> ParserIO (Module ())
file path = do
    assertNoCycle path
    visited <- stateVisited <$> getState
    case Map.lookup path visited of
        Nothing -> do
            file   <- liftIO . T.readFile $ path
            result <- liftState $ P.runParserT (module' path) path file
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

-- Parser for modules. The job of file parser is delegated here.
module' :: FilePath -> ParserIO (Module())
module' path = do
    clearLocalOperators
    skipWhitespace
    (name, exports) <- moduleHeader <?> "module declaration"
    -- TODO: store the full absolute module name instead
    checkModuleName name
    withStack name path $ do
        imports  <- P.many $ 
            located (import' <?> "module import") >>= importFile
        mapM_ importOperators imports
        definitions <- catMaybes <$> (P.many $ topLevelDef)
        skipWhitespace >> P.eof
        exportOperators exports -- at the end we update the operator table
        return $ Module (ModuleId [] name) path imports exports definitions
  where
    -- Given the import information locate and parse a submodule.
    importFile :: Located RawImport -> ParserIO (Import ())
    importFile (Located position (id, alias, identifiers)) = do
        let (ModuleId prefix mod) = id
        basePath <- getCurrentPath
        path     <- findModulePath basePath prefix mod
        mod      <- file path
        return $ Import mod id alias position identifiers
    -- Brings operators from a module into the current scope.
    importOperators :: Import a -> ParserIO ()
    importOperators importedModule = do
        let hasAlias    = isJust . importAlias $ importedModule
            path        = modulePath . importMod $ importedModule
            identifiers = importIds importedModule
            pos         = importLoc importedModule
        visible  <- (concat . Map.lookup path . stateExportedOps) <$> getState
        addLocalOperators pos $ processImports path identifiers visible hasAlias
    -- Given the path and list of imports figure out what operators need to
    -- be imported into the current operator table. If the import has an alias
    -- then the operators have to be explicitly imported to be included.
    processImports :: FilePath 
                   -> (Maybe [Located ImportedValue]) 
                   -> [(CustomOperator, Priority, Fixity)]
                   -> Bool
                   -> [(CustomOperator, Priority, Fixity)]
    processImports _ Nothing visible True = []
    processImports _ Nothing visible False = visible
    processImports path (Just imports) visible _ =
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
    checkModuleName :: ModName -> ParserIO ()
    checkModuleName name@(ModName name')
        | pathName path /= name' = fail . T.unpack $ 
            "module name \"" <> name' <> "\" doesn't match the file name"
        | otherwise = return ()
      where
        pathName = T.pack . dropExtension . takeFileName
    -- Adds all locally defined operators that are supposed to be exported to
    -- the global operator export table.
    exportOperators :: Maybe [Located ImportedValue] -> ParserIO ()
    exportOperators Nothing = do 
        locals <- stateLocalOps <$> getState
        exportOperatorsAux $ unwrapOperator <$> locals
    exportOperators (Just exports) = exportOperatorsAux $ 
        exports >>= (strip . unlocated)
    -- Convert a ImportedValue wrapper into Text identifier(s).
    strip :: ImportedValue -> [T.Text]
    strip (ImportedIdentifier (Identifier id)) = [id]
    strip (ImportedType _ Nothing) = []
    strip (ImportedType _ (Just constructors)) = 
        map (\(ConstructorName name) -> name) constructors
    -- Helper functions that exports operators stripped from all
    -- of the wrappers.
    exportOperatorsAux :: [T.Text] -> ParserIO ()
    exportOperatorsAux exports = do
        state <- getState
        let opTable   = stateCurrentOps state
            opList    = foldr (++) [] opTable
            globalOps = stateExportedOps state
            exports'  = filter (isOp opList) exports
        insertOpExports opTable opList exports' globalOps []
        clearLocalOperators
      where
        -- Checks if something is an operator.
        -- It must either have an operator character at the beginning or be
        -- defined in the local operator table in case of prefix identifiers.
        isOp :: [CustomOperator] -> T.Text -> Bool
        isOp table name = (T.head name `elem` (':' : operatorCharsList))
                        || name `elem` (unwrapOperator <$> table)
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
    -- Remove the local operator table from the parser state.
    clearLocalOperators :: ParserIO ()
    clearLocalOperators = do
        state <- getState
        putState $ state { stateCurrentOps   = Map.empty
                         , stateLocalOps     = [] 
                         , stateLocalTypeOps = Map.empty
                         }

-- Parser for module declarations.
moduleHeader :: ParserIO (ModName, Maybe [Located ImportedValue])
moduleHeader = do
    keyword "module"
    name    <- uppercaseName <?> "module name"
    exports <- moduleIdentifierList
    return (ModName name, exports)

    -- Parser for a single import statement. It does not read any new files.
import' :: ParserIO RawImport
import' = do
    keyword "import"
    name  <- qualifiedModuleName <?> "module name"
    list  <- moduleIdentifierList
    alias <- P.optional $ keyword "as" *> (uppercaseName <?> "module synonym")
    return (name, ModName <$> alias, list)

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

-- Parser for import lists. Despite the name it is also used to parse
-- the export lists. The lists are optional so it returns Maybe.
moduleIdentifierList :: ParserIO (Maybe [Located ImportedValue])
moduleIdentifierList = P.optional list
  where
    list = separatedList "{" "}" (located exportElem) ","
    -- Parser for single imported/exported thing (identifier or a type name
    -- (possibly with constructors)).
    exportElem :: ParserIO ImportedValue
    exportElem = (ImportedIdentifier . Identifier <$> prefixIdentifier)
             <|> (uncurry ImportedType <$> typeImport)
    -- Parser for type imports. Type imports consist of type name optionally
    -- followed by the explicit type constructor list. [] is a valid type name.
    typeImport :: ParserIO (TypeName, Maybe [ConstructorName])
    typeImport = do
        typeName     <- TypeName <$> typeName
        constructors <- P.optional constructorList
        return (typeName, constructors)
    -- Parser for lists of exported/imported type constructors.
    constructorList :: ParserIO [ConstructorName]
    constructorList = separatedList "{" "}" constructorName "," 
        <?> "constructor list"

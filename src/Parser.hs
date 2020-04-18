{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec ((<?>))
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, forM, when, mapM_, guard)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.Void (Void)
import Utility (Position(..), findM)
import GetOpt (Options(..), ModulePath(..), getOptions)
import qualified Data.Map as Map
import Syntax as AST
import System.FilePath.Posix ( takeDirectory
                             , takeFileName
                             , dropExtension
                             , (</>)
                             , (<.>)
                             )

-- The used monad stack looks like this:
--   ParsecT ⊃ StateT ⊃ ReaderT ⊃ IO
-- 
-- * IO is needed to open additional files during parsing.
-- * Reader is useful to access the command line arguments.
-- * State is required so that we can easily manipulate operator tables.
-- * Parsec is used for parsing (duh)

-- Monad allowing to read command line arguments and perform IO actions.
type GetOptIO = ReaderT Options IO
-- GetOptIO monad enriched with the stateful computations. The state is used
-- to store operator tables and parsed module lists.
type StateIO  = StateT ParserState GetOptIO
-- StateIO monad enriched with the parsing abilities.
type ParserIO = P.ParsecT Void T.Text StateIO

-- Shortuct for parsing errors. We don't use the custom error component.
type Errors   = P.ParseErrorBundle T.Text Void

-- State used by StateT in the parser.
data ParserState = ParserState
    { stateCurrentOps   :: LocalOperators
    -- ^ operators visible in the currently parsed file
    , stateLocalTypeOps :: Map.Map TypeName [CustomOperator]
    -- ^ operator constructors visible in the currently parser dile
    , stateLocalOps     :: [CustomOperator]
    -- ^ list of operators defined within the current module
    , stateExportedOps  :: GlobalOperators
    -- ^ operators that are exported by some files
    , stateVisited      :: Map.Map FilePath (Module ())
    -- ^ modules that were already parsed which allows us to parse them once
    , stateModuleStack  :: [(ModName, FilePath)]
    -- ^ stack used to keep track of current file path and to detect cycles
    }
  deriving Show

type LocalOperators  = Map.Map (Priority, Fixity) [CustomOperator]
type GlobalOperators = Map.Map FilePath [(CustomOperator, Priority, Fixity)]

-- Custom operator priority. Should be be in range 1..10.
type Priority = Int

data CustomOperator
    = ConstrOperator T.Text
    | PrefixConstrOperator T.Text
    | VarOperator T.Text
    | PrefixVarOperator T.Text
  deriving (Show, Ord, Eq)

-- Fixity of a custom operator.
data Fixity
    = LeftFix
    | RightFix
    | NoneFix
  deriving (Show, Ord, Eq)

-- Single import statement before being actually imported.
type RawImport = (ModuleId, Maybe ModName, Maybe [Located ImportedValue])
--                 ^              ^                   ^
--             imported module  alias         list of imported identifiers

-- TODO: decide what the path should be
-- Path to the folder with the standard library
stdLibraryDir :: FilePath
stdLibraryDir = undefined

-- File extension for Zoria files
fileExtension :: String
fileExtension = "zo"

-- Name of the "Prelude" module of the standard library
preludeName :: FilePath
preludeName = "Core" 

-- Path to the "Prelude" module of the standard library
stdPreludePath :: FilePath
stdPreludePath = stdLibraryDir </> preludeName <.> fileExtension

-- Fresh state for the parser's internal StateT monad
newParserState :: ParserState
newParserState = ParserState
    { stateCurrentOps   = Map.empty
    , stateLocalTypeOps = Map.empty
    , stateLocalOps     = []
    , stateExportedOps  = Map.empty
    , stateVisited      = Map.empty
    , stateModuleStack  = []
    }

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
    visited <- stateVisited <$> lift get
    case Map.lookup path visited of
        Nothing -> do
            fileContents <- liftIO . T.readFile $ path
            result       <- lift $ P.runParserT (module' path) path fileContents
            -- TODO: handle errors with 'region'
            mod   <- handleError result
            state <- lift get
            lift $ put state { stateVisited = Map.insert path mod visited }
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
        pathStack <- map snd . stateModuleStack <$> lift get
        when (path `elem` pathStack) $
            fail $ "detected cyclic dependency: file " 
                ++ show path 
                ++ " was previously included in a module it depends on"
            -- TODO: ^ handle it better and find the actual module name

-- Parser for modules. The job of file parser is delegated here.
module' :: FilePath -> ParserIO (Module())
module' path = do
    clearLocalOperators
    skipWhitespace
    (name, exports) <- moduleHeader <?> "module declaration"
    -- TODO: store the full absolute module name instead
    checkModuleName name
    withStack name path $ do
        rawImports  <- P.many $ located (import' <?> "module import")
        imports     <- forM rawImports importFile
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
        visible  <- (concat . Map.lookup path . stateExportedOps) <$> lift get
        addLocalOperators $ processImports path identifiers visible hasAlias
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
    addLocalOperators :: [(CustomOperator, Priority, Fixity)] -> ParserIO ()
    addLocalOperators = mapM_ defineOperator
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
        locals <- stateLocalOps <$> lift get
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
        state <- lift get            
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
            state <- lift get
            lift $ put state { stateExportedOps = newGlobals }
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
        state <- lift get
        lift $ put state { stateCurrentOps = Map.empty
                         , stateLocalOps   = [] 
                         }

unwrapOperator :: CustomOperator -> T.Text
unwrapOperator (ConstrOperator op) = op
unwrapOperator (PrefixConstrOperator op) = op
unwrapOperator (VarOperator op) = op
unwrapOperator (PrefixVarOperator op) = op

-- Function which adds an operator declaration to the local operator table.
defineOperator :: (CustomOperator, Priority, Fixity) -> ParserIO ()
defineOperator (op, priority, fixity) = do
    state @ ParserState { stateCurrentOps = visible } <- lift get
    let previousOps   = concat $ Map.lookup (priority, fixity) visible
        updatedTable  = Map.insert (priority, fixity) (op : previousOps) visible
        updatedLocals = op : (stateLocalOps state)
    assertUndefined op previousOps
    lift $ put state { stateCurrentOps = updatedTable 
                     , stateLocalOps   = updatedLocals
                     }
  where
    -- Assert that we are not overwritting some other custom operator.
    assertUndefined :: CustomOperator -> [CustomOperator] -> ParserIO ()
    assertUndefined op ops = when (op `elem` ops) $ fail $
        "illegal redeclaration of the operator " ++ prettyPrintCustomOp op

-- Perform some parsing computation inside a stack frame. On the stack we store
-- module names so that parser knows which module is parsed at the moment
-- and we can detect cycles.
withStack :: ModName -> FilePath -> ParserIO a -> ParserIO a
withStack name path parser = do
    pushModulePath name path
    result <- parser
    popModulePath
    return result

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

-- Parser for a single import statement. It does not read any new files.
import' :: ParserIO RawImport
import' = do
    keyword "import"
    name  <- qualifiedModuleName <?> "module name"
    list  <- moduleIdentifierList
    alias <- P.optional $ keyword "as" *> (uppercaseName <?> "module synonym")
    return (name, ModName <$> alias, list)

-- Get the path of the currently parsed file.
getCurrentPath :: ParserIO FilePath
getCurrentPath = do
    state <- lift get
    return $ snd . head . stateModuleStack $ state

-- Parser for qualified module names (e.g. Data\Map)
qualifiedModuleName :: ParserIO ModuleId
qualifiedModuleName = do
    prefix <- P.many $ P.try $ (uppercaseName) <* sep
    name   <- uppercaseName
    return $ ModuleId (ModName <$> prefix) (ModName name) 
  where
    sep = P.char '\\' <?> "scope operator \"\\\""

-- Parser for module declarations.
moduleHeader :: ParserIO (ModName, Maybe [Located ImportedValue])
moduleHeader = do
    keyword "module"
    name    <- uppercaseName <?> "module name"
    exports <- moduleIdentifierList
    return (ModName name, exports)

-- Add the currently parsed module to the stack in the parser state.
pushModulePath :: ModName -> FilePath -> ParserIO ()
pushModulePath name path = do
    state <- lift get
    lift $ put state { stateModuleStack = (name, path) : stateModuleStack state}

-- Remove the most recent entry on the parser state stack.
popModulePath :: ParserIO ()
popModulePath = do
    state <- lift get
    lift $ put state { stateModuleStack = tail . stateModuleStack $ state }

-- Pretty prints a custom operator.
prettyPrintCustomOp :: CustomOperator -> String
prettyPrintCustomOp (ConstrOperator op) = '`' : T.unpack op ++ "`"
prettyPrintCustomOp (PrefixConstrOperator op) = '(' : T.unpack op ++ ")"
prettyPrintCustomOp (VarOperator op) = '`' : T.unpack op ++ "`"
prettyPrintCustomOp (PrefixVarOperator op) = '(' : T.unpack op ++ ")"

-- Check whether some custom operator is a prefix operator (e.g. `elem`)
isPrefixOp :: CustomOperator -> Bool
isPrefixOp (PrefixConstrOperator _) = True
isPrefixOp (PrefixVarOperator _)    = True
isPrefixOp _ = False

-- Checks whether some custom operators is an infix operator (e.g. ::, >>=)
isInfixOp :: CustomOperator -> Bool
isInfixOp = not . isPrefixOp

-- Checks whether some custom operator is a constructor operator
isConstructorOp :: CustomOperator -> Bool
isConstructorOp (PrefixConstrOperator _) = True
isConstructorOp (ConstrOperator _) = True
isConstructorOp _ = False

-- Checks whether some custom operator is a normal operator
isNormalOp :: CustomOperator -> Bool
isNormalOp = not . isConstructorOp

-- Get the operators with given priority and fixity that are defined in
-- the current environment.
getOperators :: Priority -> Fixity -> ParserIO [CustomOperator]
getOperators priority fixity = do
    table <- stateCurrentOps <$> lift get
    return . concat $ Map.lookup (priority, fixity) table

-- Function which given a predicate on custom operators creates a parser that
-- matches this kind of operators with the given priority and fixity.
kindOfOperator :: (CustomOperator -> Bool) 
                -> Priority 
                -> Fixity 
                -> ParserIO T.Text
kindOfOperator kind priority fixity = do
    -- TODO: try to parse every operator to be able to give "undefined" error
    operators <- filter kind <$> getOperators priority fixity
    let infixOps  = unwrapOperator <$> filter isInfixOp operators
        prefixOps = unwrapOperator <$> filter isPrefixOp operators
    infixOp infixOps <|> P.try (prefixOp prefixOps)
  where
    infixOp :: [T.Text] -> ParserIO T.Text
    infixOp ops = P.choice (map operatorSymbol ops) <|> undefinedOperator
      where
        operatorSymbol :: T.Text -> ParserIO T.Text
        operatorSymbol op = 
            P.try (symbol op <* P.notFollowedBy operatorCharOrColon)
        undefinedOperator :: ParserIO a
        undefinedOperator = do
            ops <- stateLocalOps <$> lift get
            op <- P.try (cond (\op -> not (op `elem` ops)) customOperator)
            fail $ "undefined operator " ++ show (unwrapOperator op)

    prefixOp :: [T.Text] -> ParserIO T.Text
    prefixOp ops = P.char '`' *> P.choice (map P.string ops) <* symbol "`"


-- Parser for constructor operators (e.g. ::, `Foo`)
constructorOperator' :: Priority -> Fixity -> ParserIO T.Text
constructorOperator' = kindOfOperator isConstructorOp

-- Parser for non-constructor operators (e.g. ++, `elem`)
operator' :: Priority -> Fixity -> ParserIO T.Text
operator' = kindOfOperator isNormalOp

-- Parser for top-level definitions (types, classes, instances, let, etc.)
topLevelDef :: ParserIO (Maybe (TopLevelDef ()))
topLevelDef = (operatorDecl $> Nothing <?> "operator declaration")
    <|> (Just . TopLevelLet <$> letDef <?> "let definition")
    <|> (Just . TopLevelLet <$> letRecDef <?> "let-rec definition")
    -- TODO: implement

-- Parser for top-level let-definitions
letDef :: ParserIO (LetDef ())
letDef = P.try letKeyword *> (LetDef <$> definition)

-- Parser for top-level let-rec-definitions
letRecDef :: ParserIO (LetDef ())
letRecDef = withPos $ \pos -> do
    P.try (keyword "let-rec")
    defs <- (pure <$> definition) <|> (separatedList "{" "}" definition ";")
    return $ LetRecDef defs pos

-- Parser for the definition following the 'let' or 'let-rec' keywords.
-- Consists of the pattern, type signature and the expression.
definition :: ParserIO (Definition ())
definition = withPos $ \pos -> do
    patterns <- P.some (namedPattern <?> "pattern")
    symbol ":"
    sig <- P.optional typeSignature <?> "type signature"
    symbol "="
    body <- expression <?> "expression"
    case desugar patterns sig body pos of
        Left err  -> fail err
        Right def -> return def
  where
    -- Function which turns function definitions into lambda definitions.
    -- e.g. 'let f x := e' => 'let f := fn x => e'
    -- Function patterns must have a variable as the leading pattern.
    desugar [p] sig body pos = Right $ Definition p sig body pos
    desugar (fun@(VarPattern (Identifier var) _ _):args) sig body pos =
        Right $ Definition fun sig (lambdify args) pos
      where
        lambdify [p]    = Lambda p body (Just var) pos ()
        lambdify (p:ps) = Lambda p (lambdify ps) (Just var) pos ()
    desugar _ _ _ _ = 
        Left "invalid function pattern (function name must be an variable)"

-- Parser for operator declarations.
operatorDecl :: ParserIO ()
operatorDecl = do
    P.try $ keyword "let-infix"
    op <- customOperator <?> "infix identifier"
    priority <- lexeme L.decimal <?> "operator precedence"
    fixity <- operatorFixity <?> "operator fixity (left, none or right)"
    assertValidPriority priority
    defineOperator (op, priority, fixity)
  where
    assertValidPriority :: Priority -> ParserIO ()
    assertValidPriority priority =
        when (priority < 1 || priority > 10) $
            fail $ "precedence out of range (got " 
                ++ show priority 
                ++ ", expected 1..10)"

-- Parser for operators in operator declarations.
customOperator :: ParserIO CustomOperator
customOperator = ConstrOperator <$> constructorOperator
             <|> VarOperator    <$> operator
             <|> PrefixConstrOperator 
                <$> (P.try $ surroundedBy "`" uppercaseName "`")
             <|> PrefixVarOperator    <$> surroundedBy "`" lowercaseName "`"

-- Parser for operator fixity in operator declarations.
operatorFixity :: ParserIO Fixity
operatorFixity = (keyword "left" $> LeftFix) 
            <|> (keyword "right" $> RightFix) 
            <|> (keyword "none"  $> NoneFix)

-- Parser for identifiers that can be appear in the prefix position 
-- (normal operators in brackets or variable names)
prefixIdentifier :: ParserIO T.Text
prefixIdentifier = lowercaseName 
               <|> (surroundedBy "(" operator ")")

-- List of lowercase words that can not be used as identifiers.
reserved :: [T.Text]
reserved = ["module", "import", "class", "instance", "let", "in", "with",
            "match", "case", "and", "or", "fn", "type", "alias", "let-rec",
            "end", "if", "then", "else", "_external", "_internal", "λ", 
            "let-infix"]

-- List of uppercase words that can not be used as identifiers.
upperReserved :: [T.Text]
upperReserved = ["True", "False"]

-- List of built-in operators that can't be used as custom operators.
reservedOperators :: [T.Text]
reservedOperators = [":", "=>", "->", "@", "=", ":="]

-- Parser for uppercase identifiers (e.g. Nothing, Maybe)
uppercaseName :: ParserIO T.Text
uppercaseName = lexeme $ do
    name <- T.pack <$> (pure (:) <*> P.upperChar <*> P.many nameChar)
    when (name `elem` upperReserved) $
        fail ("unexpected keyword " ++ show name)
    return name

-- Parser for lowercase identifiers (e.g. map)
lowercaseName :: ParserIO T.Text 
lowercaseName = lexeme $ do
    name <- T.pack <$> (pure (:) <*> P.lowerChar <*> P.many nameChar)
    when (name `elem` reserved) $
        fail ("unexpected keyword " ++ show name)
    return name

-- Parser for operators starting with ':' except the ':' operator.
-- These operators can be used as constructor names only.
constructorOperator :: ParserIO T.Text
constructorOperator = lexeme $ do
    op <- T.pack <$> (pure (:) <*> P.char ':' <*> P.some operatorCharOrColon)
    when (op `elem` reservedOperators) $
        fail ("unexpected reserved operator " ++ show op)
    return op

-- Parser for operators that don't start with ':'.
operator :: ParserIO T.Text
operator = lexeme $ do
    op <- T.pack <$> (pure (:) <*> operatorChar <*> P.many operatorCharOrColon)
    when (op `elem` reservedOperators) $
        fail ("unexpected reserved operator " ++ show op)
    return op

-- Parser for a character that is allowed within non-constructor operators.
operatorChar :: ParserIO Char
operatorChar = P.oneOf operatorCharsList

-- List of allowed operator characters. Note that it does not include ':'.
operatorCharsList :: [Char]
operatorCharsList = ['=', '+', '-', '*', '.', '/', '!', '~', '^',
                     '$', '%', '&', '?', '>', '<', '@', '|']

-- Parser for operator characters extended with ':' character.
operatorCharOrColon :: ParserIO Char
operatorCharOrColon = operatorChar <|> P.char ':'

-- Parser combinator that transforms the given parser into a parser that also
-- stores the parsed thing's position.
located :: ParserIO a -> ParserIO (Located a)
located parser = withPos $ \pos -> Located pos <$> parser

-- Parser for lists starting with "start", ending with "end", separated by
-- "sep" and with elements that match the given parser "elem".
separatedList :: T.Text -> T.Text -> ParserIO a -> T.Text -> ParserIO [a]
separatedList start end elem sep = P.between start' end' (P.sepBy elem sep')
  where
    start' = symbol start
    end'   = symbol end
    sep'   = symbol sep

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

-- Parser for type names (e.g. Maybe, Int, [])
typeName :: ParserIO T.Text
typeName = (uppercaseName <|> symbol "[]") <?> "type name"

typeVariable :: ParserIO T.Text
typeVariable = lowercaseName <?> "type variable"

-- Parser combinator that turns a parser of something into a parser of the
-- same thing but between 'left' and 'right' separators (e.g parenthesis).
surroundedBy :: T.Text -> ParserIO a -> T.Text -> ParserIO a
surroundedBy left parser right = symbol left *> parser <* symbol right

-- Parser for constructor names (e.g. (::), Just, [])
constructorName :: ParserIO ConstructorName
constructorName = ConstructorName <$> 
    (uppercaseName <|> surroundedBy "(" constructorOperator ")" <|> symbol "[]")

-- Parser for keywords. It makes sure the keyword is not a part of a longer
-- identifier.
keyword :: T.Text -> ParserIO ()
keyword kw = keywordParserIO <?> ("keyword " ++ show kw)
  where
    keywordParserIO = (lexeme . P.try) $ P.string kw *> P.notFollowedBy nameChar

-- Parser for 'let' keyword. Normal keyword parser won't suffice since
-- 'let' can be followed by '-'.
letKeyword :: ParserIO ()
letKeyword = keyword "let" >> P.notFollowedBy (P.char '-')

-- Parser for explicit type signatures.
typeSignature :: ParserIO TypeSig
typeSignature = do
    context <- P.optional . P.try $ constraints <* symbol "=>"
    sig     <- type'
    return $ TypeSig (concat context) sig

-- Parser for constraints in type signatures.
constraints :: ParserIO [Constraint]
constraints = pure <$> singleConstraint
          <|> separatedList "(" ")" singleConstraint ","
  where
    singleConstraint :: ParserIO Constraint
    singleConstraint = do
        className  <- TypeName <$> typeName
        classParam <- TypeVar  <$> typeVariable
        return $ Constraint className classParam

-- Parser for types.
type' :: ParserIO Type
type' = P.try functionType
    <|> paramType

-- Parser for function types (e.g. a -> b)
functionType :: ParserIO Type
functionType = do
    from <- paramType
    to   <- P.optional (symbol "->" >> functionType)
    return $ case to of
        Nothing -> from
        Just to -> FunctionType from to

-- Parser for parametrized types (e.g. Maybe a, f a b c)
paramType :: ParserIO Type
paramType = P.try polyParamType <|> P.try concreteParamType <|> atomicType
  where
    polyParamType :: ParserIO Type
    polyParamType = do
        t <- TypeVar <$> typeVariable
        args <- P.many atomicType 
        return $ case args of
            [] -> TypeVariable t
            _  -> PolymorphicParamType t args
    concreteParamType :: ParserIO Type
    concreteParamType = do
        t <- TypeName <$> typeName
        args <- P.many atomicType
        return $ case args of
            [] -> NonPrimType t
            _  -> ParamType t args

-- Parser for types that are atomic (like type names or types in brackets)
atomicType :: ParserIO Type
atomicType = (P.try arrayType <?> "array type")
        <|> (P.try listType <?> "list type")
        <|> (PrimitiveType <$> (P.try primType <?> "type name"))
        <|> (NonPrimType . TypeName <$> typeName <?> "type name")
        <|> (TypeVariable . TypeVar <$> typeVariable <?> "type variable")
        <|> P.try (P.between (symbol "(") (symbol ")") type')
        <|> tupleType <?> "tuple type"

-- Parser for arrays types (e.g. [<Int>], [[<Float>]])
arrayType :: ParserIO Type
arrayType = ArrayType <$> (symbol "[<" *> type' <* symbol ">]")

-- Parser for tuples of types. They must be at least of length 2.
tupleType :: ParserIO Type
tupleType = TupleType <$> separatedList "(" ")" type' ","

-- Parser for list type syntax sugar. [] is a normal valid type name, but
-- there is a synonym [a] which is the same as ([] a)
listType :: ParserIO Type
listType = do
    t <- P.between (symbol "[") (symbol "]") type'
    return $ ParamType (TypeName "[]") [t]

-- Parser for primitive types like Int or ()
primType :: ParserIO PrimType
primType = keyword "Int" $> IntT
       <|> keyword "Float" $> FloatT
       <|> keyword "String" $> StringT
       <|> keyword "Bool" $> BoolT
       <|> symbol "(" *> pure UnitT <* symbol ")"
       <|> P.try (keyword "Char") $> CharT
       <|> keyword "CPtr" $> CPtrT

-- Parser for expressions.
expression :: ParserIO (Expr ())
expression = P.label "expression" $
             letIn
         <|> conditionalExpr
         <|> patternMatching 
         <|> lambdaExpr
         <|> annotatedExpr

-- Parser for 'let ... in' and 'let-rec ... in' expressions.
letIn :: ParserIO (Expr ())
letIn = do
    def <- letDef <|> letRecDef
    keyword "in"
    expr <- expression <?> "expression"
    return $ LetIn def expr ()

-- Parser for 'if ... then ... else' expressions.
conditionalExpr :: ParserIO (Expr ())
conditionalExpr = withPos $ \pos -> do
    P.try $ keyword "if"
    condition <- annotatedExpr
    keyword "then"
    consequence <- expression
    keyword "else"
    alternative <- expression
    return $ If condition consequence alternative pos ()

-- Parser for 'match ... with case ...' expressions
patternMatching :: ParserIO (Expr ())
patternMatching = withPos $ \pos -> do
    P.try $ keyword "match"
    expr <- annotatedExpr
    keyword "with"
    cases <- P.some matchCase
    return $ Match expr cases pos ()

-- Parser for a single 'case' pattern matching clause.
matchCase :: ParserIO (MatchCase ())
matchCase = do
    P.try $ keyword "case"
    p <- pattern <?> "pattern"
    keyword "=>"
    expr <- expression <?> "expression"
    return $ MatchCase p expr

-- Parser for lambda expressions (e.g. fn x => x)
lambdaExpr :: ParserIO (Expr ())
lambdaExpr = withPos $ \pos -> do
    P.try $ (keyword "fn" <|> keyword "λ")
    args <- P.some pattern <?> "argument patterns"
    symbol "=>"
    body <- expression <?> "body expression"
    return $ desugar args body pos
  where
    -- Function that desugars multi-parameter lambdas into unary lambdas
    desugar :: [Pattern ()] -> Expr () -> Position -> Expr ()
    desugar [arg] expr pos = Lambda arg expr Nothing pos ()
    desugar (arg:args) expr pos = 
        Lambda arg (desugar args expr pos) Nothing pos ()

-- Parser for expressions with explicit type sygnatures or for expressions
-- with higher precedence.
annotatedExpr :: ParserIO (Expr ())
annotatedExpr = withPos $ \pos -> do
    expr <- logicalOr <?> "expression"
    sig  <- P.hidden $ P.optional $ (P.try $ symbol ":") >> typeSignature
    return $ case sig of
        Nothing  -> expr
        Just sig -> AnnotatedExpr expr sig pos ()

-- Parser for expressions with precedence at least the same as the precedence
-- of 'or' expressions.
logicalOr :: ParserIO (Expr ())
logicalOr = do
    lhs <- logicalAnd
    logicalOrAux lhs
  where
    logicalOrAux :: Expr () -> ParserIO (Expr ())
    logicalOrAux acc = withPos $ \pos -> do
        hasOp <- check $ symbol "or"
        if not hasOp
            then return acc
            else do 
                rhs <- logicalAnd <?> "expression"
                logicalOrAux $ Or acc rhs pos ()

-- Parser for expressions with precedence at least as high as 'and' expressions.
logicalAnd :: ParserIO (Expr ())
logicalAnd = do
    lhs <- opExpr 10 NoneFix
    logicalAndAux lhs
  where
    logicalAndAux :: Expr () -> ParserIO (Expr ())
    logicalAndAux acc = withPos $ \pos -> do
        hasOp <- check $ symbol "and"
        if not hasOp
            then return acc
            else do
                rhs <- opExpr 10 NoneFix <?> "expression"
                logicalAndAux $ And acc rhs pos ()

-- Parser for any kind of infix operator which returns the operator
-- as either Var or Constructor expression.
exprOperator :: Priority -> Fixity -> ParserIO (Expr ())
exprOperator priority fixity = P.label "infix operator" $
    P.try constrOp <|> normalOp
  where
    normalOp :: ParserIO (Expr ())
    normalOp = withPos $ \pos -> do
        op <- kindOfOperator isNormalOp priority fixity
        return $ Var (Identifier op) pos ()
    constrOp :: ParserIO (Expr ())
    constrOp = withPos $ \pos -> do
        op <- kindOfOperator isConstructorOp priority fixity
        return $ Constructor (ConstructorName op) pos ()

-- Parser for expressions with (custom) operators.
-- There is a lot of repetition but it's not possible to abstract into a
-- single definition since there are subtle differences between clauses.
opExpr :: Priority -> Fixity -> ParserIO (Expr ())
-- opExpr 0 _ = application --TODO: add undeclared operator check chere
opExpr 0 _ = application
opExpr priority NoneFix = do
    lhs <- nextPriorityExpr <?> "expression"
    rhs <- P.optional $ pure (,)
        <*> (exprOperator priority NoneFix <?> "operator")
        <*> (nextPriorityExpr <?> "expression")
    return $ case rhs of
        Just (op, rhs) -> App (App op lhs ()) rhs ()
        Nothing        -> lhs
  where
    nextPriorityExpr = opExpr priority RightFix
opExpr priority RightFix = do
    lhs <- opExpr priority LeftFix <?> "expression"
    rhs <- P.optional $ pure (,)
        <*> (exprOperator priority RightFix <?> "operator")
        <*> (opExpr priority RightFix <?> "expression")
    return $ case rhs of
        Just (op, rhs) -> App (App op lhs ()) rhs ()
        Nothing        -> lhs
opExpr priority LeftFix = do
    lhs <- nextPriorityExpr <?> "expression"
    leftfixExprAux lhs
  where
    nextPriorityExpr = opExpr (priority - 1) NoneFix
    leftfixExprAux :: Expr () -> ParserIO (Expr ())
    leftfixExprAux acc = do
        op <- (P.optional $ exprOperator priority LeftFix) <?> "operator"
        case op of
            Nothing -> return acc
            Just op -> do
                rhs <- nextPriorityExpr <?> "expression"
                leftfixExprAux $ App (App op acc ()) rhs ()

-- Parser for function application. Function application can be treated like
-- invisible leftfix operator with priority 0 (highest).
application :: ParserIO (Expr ())
application = do
    head <- atomicExpr <?> "expression"
    args  <- P.many atomicExpr <?> "argument expressions"
    return $ case args of
        [] -> head
        _  -> foldl applify head args
  where
    applify :: Expr () -> Expr () -> Expr ()
    applify funct arg = App funct arg ()

-- Parser for expressions with the highest precedence (literals / bracketed
-- expressions)
atomicExpr :: ParserIO (Expr ())
atomicExpr = withPos $ \pos -> 
    ((\e -> Primitive e pos ()) <$> P.try primExpr)
    <|> internal
    <|> external
    <|> stringExpr
    <|> P.try listLit
    <|> arrayLit
    <|> blockExpr
    <|> P.try qualifiedName
    <|> P.try varExpr
    <|> P.try constructorExpr
    <|> P.try (P.between (symbol "(") (symbol ")") expression)
    <|> tupleExpr

-- Returns a readable error in case of an undefined operator.
operatorFail :: ParserIO a
operatorFail = do
    op <- unwrapOperator <$> P.try customOperator
    fail $ "unknown operator " ++ show op 

-- Used by blockExpr parser.
data BlockElem
    = BlockLet (LetDef ()) Position
    | BlockExpr (Expr ()) Position

-- Parser for block expressions.
blockExpr :: ParserIO (Expr ())
blockExpr = withPos $ \pos -> do
    symbol "{"
    exprs <- P.many $ (blockLet <* symbol ";") 
                  <|> P.try (blockExpr <* symbol ";")
    final <- withPos $ \pos -> P.option (implicitUnit pos) (P.try blockExpr)
    symbol "}"
    return $ Block (desugar $ exprs ++ [final]) pos ()
  where
    -- Parser for block let definitions.
    blockLet :: ParserIO BlockElem
    blockLet = withPos $ \pos -> 
        (flip BlockLet pos) <$> (letDef <|> letRecDef) <?> "let definition"
    -- Parser for block expressions wrapped in the BlockElem type.
    blockExpr :: ParserIO BlockElem
    blockExpr = withPos $ \pos -> 
        (flip BlockExpr pos) <$> expression <?> "expression"
    -- Impliit unit placed after the final trailing semicolon in the block.
    implicitUnit :: Position -> BlockElem
    implicitUnit pos = BlockExpr (Primitive UnitLit pos ()) pos
    -- Function that turns a linear list of expressions into a proper tree.
    desugar :: [BlockElem] -> [Expr ()]
    desugar [] = []
    desugar ((BlockLet def pos):exprs) = [LetIn def subBlock ()]
      where
        subBlock = Block (desugar exprs) pos ()
    desugar ((BlockExpr expr pos):exprs) = expr : desugar exprs

-- Parsers for names with explicit namespace (e.g. Foo\bar, Bar\Foo, Foo\(++))
qualifiedName :: ParserIO (Expr ())
qualifiedName = withPos $ \pos -> do
    namespace <- ModName <$> (P.try $ uppercaseName <* P.char '\\')
    P.try (qualifiedConstructor namespace pos) <|> qualifiedVar namespace pos
  where
    qualifiedConstructor :: ModName -> Position -> ParserIO (Expr ())
    qualifiedConstructor mod pos = do 
        name <- constructorName
        return $ QualifiedConstructor mod name pos ()
    qualifiedVar :: ModName -> Position -> ParserIO (Expr ())
    qualifiedVar mod pos = do
        name <- Identifier <$> prefixIdentifier
        return $ QualifiedVar mod name pos ()

-- Parser for identifiers (e.g. x, (++))
varExpr :: ParserIO (Expr ())
varExpr = withPos $ \pos -> do
    var <- Identifier <$> prefixIdentifier
    return $ Var var pos ()

-- Parser for identifiers that are constructors (e.g. Nothing, (::))
constructorExpr :: ParserIO (Expr ())
constructorExpr = withPos $ \pos -> do
    constr <- constructorName
    return $ Constructor constr pos ()

-- Parser for tuples of expressions (e.g. (2,'1',"3",7.0))
tupleExpr :: ParserIO (Expr ())
tupleExpr = withPos $ \pos -> do
    elems <- separatedList "(" ")" expression ","
    return $ Tuple elems pos ()

-- Parser for array literals (e.g. [<1,2,3>])
arrayLit :: ParserIO (Expr ())
arrayLit = withPos $ \pos -> do
    elems <- separatedList "[<" ">]" expression ","
    return $ Array elems pos ()

-- Parser for both normal and format strings.
stringExpr :: ParserIO (Expr ())
stringExpr = withPos $ \pos -> unwrap pos . filterEmpty <$> formatString 
  where
    unwrap :: Position -> [FormatExpr ()] -> Expr ()
    unwrap pos [] = Primitive (StringLit "") pos ()
    unwrap pos [FmtStr str] = Primitive (StringLit str) pos ()
    unwrap pos chunks = FormatString chunks pos ()
    formatString :: ParserIO [FormatExpr ()]
    formatString = 
        P.char '"' *> P.some (inlineExpr <|> stringChunk) <* symbol "\""
    stringChunk :: ParserIO (FormatExpr ())
    stringChunk = FmtStr . T.pack <$> P.some chunkChar
    chunkChar :: ParserIO Char
    chunkChar = 
        P.lookAhead (P.satisfy (\c -> c /= '{' && c /= '"')) *> L.charLiteral
    inlineExpr :: ParserIO (FormatExpr ())
    inlineExpr = 
        FmtExpr <$> (symbol "{" *> (expression <?> "expression") <* P.char '}')
    filterEmpty :: [FormatExpr ()] -> [FormatExpr ()]
    filterEmpty = filter notEmptyStr
      where
        notEmptyStr (FmtStr "") = False
        notEmptyStr _           = True

-- Parser for list literal expressions. Note, that they are just a syntax
-- sugar for :: and [] constructors.
listLit :: ParserIO (Expr ())
listLit = withPos $ \pos -> do
    xs <- separatedList "[" "]" expression ","
    return $ foldr (applify pos) (Constructor nilConstructor pos ()) xs
  where
    applify :: Position -> Expr () -> Expr () -> Expr ()
    applify pos x xs = 
        App (App (Constructor consConstructor pos ()) x ()) xs ()

-- Parser for the '_internal' built-in names.
internal :: ParserIO (Expr ())
internal = withPos $ \pos -> do
    P.try $ keyword "_internal"
    id <- Identifier <$> lowercaseName
    return $ Internal id pos ()

-- Parser for the '_external' FfI calls.
external :: ParserIO (Expr ())
external = withPos $ \pos -> do
    P.try $ keyword "_external"
    path <- T.unpack <$> string
    name <- string
    return $ External path name pos ()

-- Parser for literal expressions.
primExpr :: ParserIO PrimExpr
primExpr = lexeme $ (P.try float <?> "float literal")
    <|> (character <?> "character")
    <|> (integer <?> "integer literal")
    <|> (StringLit <$> string <?> "string literal")
    <|> P.try boolean
    <|> (unit <?> "()")

-- Parser for floating point numbers.
float :: ParserIO PrimExpr
float = L.signed (pure ()) L.float >>= return . FloatLit

-- Parser for integers. Decimal, octal, hex and binary literals are supported.
-- The numbers must be in range [-2^63 .. 2^63 - 1] or else the parsing fails.
integer :: ParserIO PrimExpr
integer = do
    num <- (L.signed (pure ()) parseInt)
    checkOverflow num
    return . IntLit . fromInteger $ num
  where
    -- Parser for a integer literal.
    parseInt :: ParserIO Integer
    parseInt = (P.try $ P.string "0o" >> L.octal)
           <|> (P.try $ P.string "0x" >> L.hexadecimal)
           <|> (P.try $ P.string "0b" >> L.binary)
           <|> L.decimal
    -- Function that checks whether the literal is within the correct range.
    checkOverflow :: Integer -> ParserIO ()
    checkOverflow i = when (i <  min || i > max) $ fail (overflowError i)
      where
        min = toInteger (minBound :: Int)
        max = toInteger (maxBound :: Int)
        overflowError int = 
            "overflowing integer literal '" ++ show int ++ "'"

-- Parser for string literals. Rejects format strings
string :: ParserIO T.Text
string = lexeme $ P.char '"' >> (T.pack <$> P.manyTill stringChar (P.char '"')) 

-- Parser for the '\{' character
escapedBracket :: ParserIO Char
escapedBracket = P.hidden $ (P.try $ P.string "\\{") $> '{'

-- Parser for characters that can appear inside normal (unformatted) strings.
-- Bare '{' is forbidden and has to be escaped with '\'.
stringChar :: ParserIO Char
stringChar = (escapedFormat <|> L.charLiteral) <?> "character"
  where
    escapedFormat :: ParserIO Char
    escapedFormat = (P.try (P.char '{') >> fail "unescaped format bracket!")
                <|> (P.string "\\{" $> '{')

-- Parser for character literals.
character :: ParserIO PrimExpr
character = (P.char '\'' *> L.charLiteral <* P.char '\'') >>= return . CharLit

-- Parser for boolean literals ('True' or 'False')
boolean :: ParserIO PrimExpr
boolean = (keyword "True" $> True) 
      <|> (keyword "False" $> False) 
      >>= return . BoolLit

-- Parser for units. Parens may be separated by whitespace.
unit :: ParserIO PrimExpr
unit = symbol "(" *> pure UnitLit <* symbol ")"

-- Parser for a single character that is allowed in identifiers.
nameChar :: ParserIO Char
nameChar = P.choice [P.alphaNumChar, P.char '\'', P.char '_']

-- Parser for patterns which can appear either in 'match ... with' 
-- or let definitions
pattern :: ParserIO (Pattern ())
pattern = infixConstructorPattern 10 NoneFix

-- Infix application of a constructor (e.g. a `Foo` b, x::xs)
infixConstructorPattern :: Priority -> Fixity -> ParserIO (Pattern ())
infixConstructorPattern 0 _ = prefixConstructorPattern
infixConstructorPattern priority NoneFix = withPos $ \pos -> do
    lhs <- infixConstructorPattern priority RightFix
    rhs <- P.optional $ pure (,) 
        <*> (P.try $ constructorOperator' priority NoneFix)
        <*> infixConstructorPattern priority RightFix
    return $ case rhs of
        Just (op, rhs) -> 
            ConstructorPattern (ConstructorName op) [lhs, rhs] pos ()
        Nothing -> lhs
infixConstructorPattern priority RightFix = withPos $ \pos -> do
    lhs <- infixConstructorPattern priority LeftFix
    rhs <- P.optional $ pure (,)
        <*> (P.try $ constructorOperator' priority RightFix)
        <*> infixConstructorPattern priority RightFix
    return $ case rhs of
        Just (op, rhs) -> 
            ConstructorPattern (ConstructorName op) [lhs, rhs] pos ()
        Nothing -> lhs
infixConstructorPattern priority LeftFix = withPos $ \pos -> do
    lhs <- infixConstructorPattern (priority - 1) NoneFix
    leftfixConstructorPattern priority lhs
  where
    -- Tail recursive parser for left-recursion elimination.
    leftfixConstructorPattern :: Priority -> Pattern () -> ParserIO (Pattern ())
    leftfixConstructorPattern priority acc = withPos $ \pos -> do
        op <- P.optional $ constructorOperator' priority LeftFix
        case op of
            Nothing -> return acc
            Just op -> do
                rhs <- infixConstructorPattern (priority - 1) NoneFix
                leftfixConstructorPattern priority $
                    ConstructorPattern (ConstructorName op) [acc, rhs] pos ()

-- Prefix application of a constructor (e.g. Just 42, (::) x xs)
prefixConstructorPattern :: ParserIO (Pattern ())
prefixConstructorPattern = P.try constructorPattern <|> namedPattern
  where
    constructorPattern :: ParserIO (Pattern ())
    constructorPattern = withPos $ \pos -> do
        constructor <- constructorName
        arguments <- P.many namedPattern
        return $ ConstructorPattern constructor arguments pos ()

-- Parser for synonym patterns (e.g. ys@(x::xs))
namedPattern :: ParserIO (Pattern ())
namedPattern = P.try namedPattern' <|> atomicPattern
  where
    namedPattern' :: ParserIO (Pattern ())
    namedPattern' = withPos $ \pos -> do
        name <- Identifier <$> prefixIdentifier
        symbol "@"
        pat <- atomicPattern
        return $ NamedPattern name pat pos ()

-- Parser for list literals. List literals are just syntax sugar for
-- :: operator and [] constructor.
listLiteralPattern :: ParserIO (Pattern ())
listLiteralPattern = withPos $ \pos -> do
    (Located pos ps) <- located $ separatedList "[" "]" pattern ","
    return $ foldPatterns ps pos
  where
    foldPatterns :: [Pattern ()] -> Position -> Pattern ()
    foldPatterns [] pos = ConstructorPattern nilConstructor [] pos ()
    foldPatterns (p:ps) pos = 
        ConstructorPattern consConstructor [p, ps'] pos ()
      where
        ps' = foldPatterns ps pos

-- Pattern expression with the highest precedence or any pattern in parenthesis.
atomicPattern :: ParserIO (Pattern ())
atomicPattern = lexeme $ 
    wildcardPattern
    <|> P.try literalPattern
    <|> P.try variablePattern
    <|> P.try (surroundedBy "(" pattern ")")
    <|> listLiteralPattern
    <|> tuplePattern

-- Tuple of patterns (e.g. (1,2), ())
tuplePattern :: ParserIO (Pattern ())
tuplePattern = withPos $ \pos -> do
    subpatterns <- separatedList "(" ")" pattern ","
    -- TODO: assert that the length is at least 2
    return $ TuplePattern subpatterns pos ()

-- Parser that parses patterns consisting of variables
variablePattern :: ParserIO (Pattern ())
variablePattern = withPos $ \pos -> do
    var <- Identifier <$> prefixIdentifier
    return $ VarPattern var pos ()

-- Parser for the '_' pattern.
wildcardPattern :: ParserIO (Pattern ())
wildcardPattern = withPos $ \pos -> symbol "_" $> WildcardPattern pos ()

-- Parser for constant patterns (e.g. 42, "string", True)
literalPattern :: ParserIO (Pattern ())
literalPattern = withPos $ \pos -> do
    expr <- primExpr
    return $ ConstPattern expr pos ()

-- Constructor name constant for the nil constructor.
nilConstructor :: ConstructorName
nilConstructor = ConstructorName "[]"

-- Constructor name constant for the cons operator.
consConstructor :: ConstructorName
consConstructor = ConstructorName "::"

-- Parser combinator that lets you easily extract the current file position.
-- The argument is a function which takes a position and as a result returns
-- some parser. The result is a parser instrumented with position checking.
withPos :: P.MonadParsec e s m => (Position -> m a) -> m a
withPos f = do
    state  <- P.getParserState
    offset <- P.getOffset
    let file = P.sourceName . P.pstateSourcePos . P.statePosState $ state
    f $ Position offset file 

-- Parser for whitespace which is supposed to be ignored.
skipWhitespace :: ParserIO ()
skipWhitespace = L.space (void P.spaceChar) lineComment blockComment
  where
    blockComment = L.skipBlockCommentNested "#:" ":#"
    -- Custom line comment parser. The L.skipLineComment wouldn't work
    -- because block and line comments share a common prefix.
    lineComment  = P.try (P.char '#' *> P.notFollowedBy ":") >> skipLine
    -- Parser which consumes all characters up to the end of a line.
    skipLine :: ParserIO ()
    skipLine = void $ P.takeWhileP (Just "character") (/= '\n')

-- Parser combinator which makes the parser also consume the trailing whitespace
lexeme :: ParserIO a -> ParserIO a
lexeme = L.lexeme skipWhitespace

-- Function which creates a parser which consumes the given string literal
-- and the trailing space.
symbol :: T.Text -> ParserIO T.Text
symbol = L.symbol skipWhitespace

-- Parser combinator that creates a parser, which returns True if the given
-- parser matches the input (and then consumes it), or False otherwise
-- (then the output remains unconsumed)
check :: ParserIO a -> ParserIO Bool
check p = P.hidden $ P.optional (P.try $ void p) >>= return . isJust

-- Parser which fails if the result of given parser does not match the 
-- predicate.
cond :: (a -> Bool) -> ParserIO a -> ParserIO a
cond predicate parser = do
    result <- parser
    guard (predicate result)
    return result

-- Computation which extracts the command line options from the reader monad.
getopt :: ParserIO Options
getopt = lift . lift $ ask

-- Prints a message with current parser position.
debug :: String -> ParserIO ()
debug str = withPos $ \(Position pos file) -> 
    liftIO . putStrLn $ show file ++ " " ++ show pos ++ ": " ++ str

-- Failure without an explicit cause
silentFail :: ParserIO ()
silentFail = void $ P.satisfy (const False)

-- Function used to simplyfy running the parser monad stack.
-- It takes the parser to be run, file name (used for errors), input string,
-- initial StateT state and command line arguments for ReaderT monad.
-- The result is an IO computation which will either parse something or
-- fail with some errors.
runParser :: ParserIO a 
          -> FilePath
          -> T.Text 
          -> ParserState 
          -> Options
          -> IO (Either Errors a)
runParser parser file input initState options = do
    (result, _) <- runReaderT reader options
    return result
  where
    state = P.runParserT parser file input
    reader = runStateT state initState

-- IO computation which parses the program starting with the file
-- specified as command line argument.
parseProgram :: IO (Program ())
parseProgram = do
    opts   <- addRootModule <$> getOptions
    result <- runParser program "" "" newParserState opts
    case result of
        Left err -> do 
            putStrLn $ P.errorBundlePretty err
            exitFailure
        Right program -> return program
  where
    -- Adds the main file directory to the list of external modules specified
    -- in command line arguments. The module root name is "\".
    addRootModule :: Options -> Options
    addRootModule opts@(Options { optModules = mods, optInputs = inputs }) = 
        opts { optModules = rootModule : mods }
      where
        rootModule = ModulePath "\\" (takeDirectory $ head inputs)

-- Helper function used to test parsers.
testParser :: Show a => ParserIO a -> T.Text -> IO ()
testParser parser input = do
    result <- parsingResult
    case result of
        Left err     -> putStrLn $ P.errorBundlePretty err
        Right result -> putStrLn $ show result
  where
    emptyOpts = Options "" [] []
    parsingResult = runParser parser "" input newParserState emptyOpts

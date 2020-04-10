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
import Control.Monad (void, forM, when, mapM_)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (fromJust)
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
    { stateCurrentOps  :: LocalOperators
    -- ^ operators visible in the currently parsed file
    , stateLocalOps    :: [CustomOperator]
    -- ^ list of operators defined within the current module
    , stateExportedOps :: GlobalOperators
    -- ^ operators that are exported by some files
    , stateVisited     :: Map.Map FilePath (Module ())
    -- ^ modules that were already parsed which allows us to parse them once
    , stateModuleStack :: [(ModName, FilePath)]
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
newParserState = ParserState Map.empty [] Map.empty Map.empty []

-- Parser of programs. It is supposed to be used once.
program :: ParserIO (AST.Program ())
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
            handleError result
        Just mod -> return mod
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
                ++ "was previously included in a module it depends on"
            -- TODO: ^ handle it better and find the actual module name

-- Parser for modules. The job of file parser is delegated here.
module' :: FilePath -> ParserIO (Module())
module' path = do
    skipWhitespace
    (name, exports) <- moduleHeader <?> "module declaration"
    -- TODO: store the full absolute module name instead
    checkModuleName name
    withStack name path $ do
        rawImports  <- P.many $ located (import' <?> "module import")
        imports     <- forM rawImports importFile
        definitions <- P.many $ topLevelDef
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
        visible  <- (concat . Map.lookup path . stateExportedOps) <$> lift get
        addLocalOperators $ processImports path identifiers visible
        return $ Import mod id alias position identifiers
    -- Given the path and list of imports figure out what operators need to
    -- be imported into the global 
    processImports :: FilePath 
                   -> (Maybe [Located ImportedValue]) 
                   -> [(CustomOperator, Priority, Fixity)]
                   -> [(CustomOperator, Priority, Fixity)]
    processImports _ Nothing visible = visible
    processImports path (Just imports) visible =
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
    -- the global operator export table. Clears the local operator table.
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

-- Function which adds an operator declaration to the operator table.
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
    externalModules <- optModules <$> getopt
    let choices = absPath : (optModPath <$> filter matching externalModules)
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
    matching mod
        | null prefix = optModName mod == unwrapName name
        | otherwise   = optModName mod == unwrapName (head prefix)
    unwrapName (ModName name) = name

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
isPrefixOp (PrefixVarOperator _) = True
isPrefixOp _ = False

-- Checks whether some custom operators is an infix operator (e.g. ::, >>=)
isInfixOp :: CustomOperator -> Bool
isInfixOp = not . isPrefixOp

-- Checks whether some custom operator is a constructor operator
isConstructorOp :: CustomOperator -> Bool
isConstructorOp (ConstrOperator _) = True
isConstructorOp (PrefixConstrOperator _) = True
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
    operators <- filter kind <$> getOperators priority fixity
    let infixOps  = unwrapOperator <$> filter isInfixOp operators
        prefixOps = unwrapOperator <$> filter isPrefixOp operators
    infixOp infixOps <|> prefixOp prefixOps
  where
    infixOp :: [T.Text] -> ParserIO T.Text
    infixOp ops = P.choice (map symbol ops)
    prefixOp :: [T.Text] -> ParserIO T.Text
    prefixOp ops = P.char '`' *> P.choice (map P.string ops) <* symbol "`"

-- Parser for constructor operators (e.g. ::, `Foo`)
constructorOperator' :: Priority -> Fixity -> ParserIO T.Text
constructorOperator' = kindOfOperator isConstructorOp

-- Parser for non-constructor operators (e.g. ++, `elem`)
operator' :: Priority -> Fixity -> ParserIO T.Text
operator' = kindOfOperator isNormalOp

-- Parser for top-level definitions (types, classes, instances, let, etc.)
topLevelDef :: ParserIO (TopLevelDef a)
topLevelDef = operatorDefinition
    -- TODO: implement
    <|> P.try globalLetDef
    <|> P.try globalLetRecDef

-- Parser for top-level let-definitions
globalLetDef :: ParserIO (TopLevelDef a)
globalLetDef = keyword "let" *> (TopLevelLet . LetDef <$> letDefBody)

-- Parser for top-level let-rec-definitions
globalLetRecDef :: ParserIO (TopLevelDef a)
globalLetRecDef = undefined

-- Parser for the definition following the 'let' keyword.
-- Consists of the pattern, type signature and the expression.
letDefBody :: ParserIO (Definition a)
letDefBody = undefined

-- Parser for operator declaration and the following let or type definition. 
-- Every declaration must be followed by the operator definition.
operatorDefinition :: ParserIO (TopLevelDef a)
operatorDefinition = undefined

-- Parser for operator fixity in operator declarations.
operatorFixity :: ParserIO Fixity
operatorFixity = (keyword "left" $> LeftFix) 
            <|> (keyword "right" $> RightFix) 
            <|> (keyword "none" $> NoneFix)

-- Parser for identifiers that can be appear in the prefix position 
-- (normal operators in brackets or variable names)
prefixIdentifier :: ParserIO T.Text
prefixIdentifier = lowercaseName 
               <|> (surroundedBy "(" operator ")")

-- Parser for identifiers that can be used in an infix way
-- (operators or identifiers surrounded by backticks)
infixIdentifier :: ParserIO T.Text
infixIdentifier = (P.try $ P.char '`' *> lowercaseName <* P.char '`')
            <|> (P.char '`' *> uppercaseName <* P.char '`')
            <|> operator
            <|> constructorOperator

-- List of lowercase words that can not be used as identifiers.
reserved :: [T.Text]
reserved = ["module", "import", "class", "instance", "let", "in", "with",
            "match", "case", "and", "or", "fn", "type", "alias", "let-rec",
            "end", "if", "then", "else", "_external", "_internal"]

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
        fail ("Keyword " ++ show name ++ " is not a valid identifier!")
    return name

-- Parser for lowercase identifiers (e.g. map)
lowercaseName :: ParserIO T.Text 
lowercaseName = lexeme $ do
    name <- T.pack <$> (pure (:) <*> P.lowerChar <*> P.many nameChar)
    when (name `elem` reserved) $
        fail ("Keyword " ++ show name ++ " is not a valid identifier!")
    return name

-- Parser for operators starting with ':' except the ':' operator.
-- These operators can be used as constructor names only.
constructorOperator :: ParserIO T.Text
constructorOperator = lexeme $ do
    op <- T.pack <$> (pure (:) <*> P.char ':' <*> P.some operatorCharOrColon)
    when (op `elem` reservedOperators) $
        fail ("Operator " ++ show op ++ " is a reserved operator!")
    return op

-- Parser for operators that don't start with ':'.
operator :: ParserIO T.Text
operator = lexeme $ do
    op <- T.pack <$> (pure (:) <*> operatorChar <*> P.many operatorCharOrColon)
    when (op `elem` reservedOperators) $
        fail ("Operator " ++ show op ++ " is a reserved operator!")
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
        typeName     <- TypeName <$> typeName <?> "type name"
        constructors <- P.optional constructorList
        return (typeName, constructors)
    -- Parser for lists of exported/imported type constructors.
    constructorList :: ParserIO [ConstructorName]
    constructorList = separatedList "{" "}" constructorName "," 
        <?> "constructor list"

-- Parser for type names (e.g. Maybe, Int, [])
typeName :: ParserIO T.Text
typeName = uppercaseName <|> symbol "[]"

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

-- Parser for literal expressions.
primExpr :: ParserIO PrimExpr
primExpr = lexeme $ (P.try float <?> "float literal")
    <|> character
    <|> (integer <?> "integer literal")
    <|> string
    <|> (P.try boolean <?> "bool literal")
    <|> (unit <?> "()")

-- Parser for variable names. Only lowercase identifiers and operators within
-- parenthesis are valid names.
{- varExpr :: ParserIO (Expr ())
varExpr = withPos $ \pos -> do
    var <- lowercaseName 
        <|> surroundedBy "(" operator ")"
    return $ Var (Identifier var) pos ()

-- Parser for constructor names as primitive expressions.
constrExpr :: ParserIO (Expr ())
constrExpr = withPos $ \pos -> do
    constr <- constructorName
    return $ Constructor constr pos ()
 -}

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

-- Parser for string literals.
-- TODO: parse format strings.
string :: ParserIO PrimExpr
string = P.char '"' 
    >> (T.pack <$> P.manyTill L.charLiteral (P.char '"')) 
    >>= return . StringLit

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
pattern = P.try (infixConstructorPattern 10 NoneFix)
    <|> namedPattern                 

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
    replacePos pos <$> leftfixConstructorPattern priority lhs
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
    -- Function that replaces the position stored within the pattern because
    -- the position from leftfixConstructorPattern is not valid for the
    -- first pattern in the sequence
    replacePos :: Position -> Pattern () -> Pattern ()
    replacePos pos (ConstructorPattern name args _ _) = 
        ConstructorPattern name args pos ()

-- Prefix application of a constructor (e.g. Just 42, (::) x xs)
prefixConstructorPattern :: ParserIO (Pattern ())
prefixConstructorPattern = withPos $ \pos -> do
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
    foldPatterns [] pos = ConstructorPattern (ConstructorName "[]") [] pos ()
    foldPatterns (p:ps) pos = 
        ConstructorPattern (ConstructorName "::") [p, ps'] pos ()
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

-- Computation which extracts the command line options from the reader monad.
getopt :: ParserIO Options
getopt = lift . lift $ ask

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
    opts   <- getOptions
    result <- runParser program "" "" newParserState opts
    case result of
        Left err  -> do 
            putStrLn $ P.errorBundlePretty err
            exitFailure
        Right program -> return program

-- Helper function used to test parsers. It runs the given parser
-- on some string.
testParser :: Show a => ParserIO a -> T.Text -> IO ()
testParser parser input = do
    result <- parsingResult
    case result of
        Left err     -> putStrLn $ P.errorBundlePretty err
        Right result -> putStrLn $ show result
  where
    emptyOpts = Options "" [] []
    parsingResult = runParser parser "" input newParserState emptyOpts
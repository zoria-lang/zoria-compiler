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
import Control.Monad (void, forM, when)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Utility (Position(..), findM)
import GetOpt (Options(..), ModulePath(..), getOptions)
import qualified Data.Map as Map
import Syntax as AST
import System.FilePath.Posix (takeDirectory, takeFileName, 
                              dropExtension, (</>), (<.>))

-- The used monad stack looks like this:
--      IO ⊂ ReaderT ⊂ StateT ⊂ ParsecT
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
    -- ^ operators defined in the currently parsed file
    , stateExportedOps :: GlobalOperators
    -- ^ operators that are exported by some files
    , stateVisited     :: Map.Map FilePath (Module ())
    -- ^ modules that were already parsed which allows us to parse them once
    , stateModuleStack :: [(ModName, FilePath)]
    -- ^ stack used to keep track of current file path and to detect cycles
    }
  deriving Show

type LocalOperators = Map.Map (Priority, Fixity) [T.Text]
type GlobalOperators = Map.Map FilePath [(T.Text, Priority, Fixity)]

-- Custom operator priority. Should be be in range 1..10.
type Priority = Int

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
newParserState = ParserState Map.empty Map.empty Map.empty []

-- Parser of programs. It is supposed to be used once.
program :: ParserIO (AST.Program ())
program = do
    opts <- show <$> getopt
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
            result <- lift $ P.runParserT (module' path) path fileContents
            -- TODO: handle errors with 'region'
            handleError result
        Just mod -> return mod
  where
    -- Handler for errors from submodules
    handleError :: Either Errors (Module ()) -> ParserIO (Module ())
    handleError (Right mod) = return mod
    handleError (Left err) = 
        liftIO $ putStrLn (P.errorBundlePretty err) >> exitFailure
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
        rawImports <- P.many $ located (import' <?> "module import")
        imports <- forM rawImports importFile
        definitions <- P.many $ topLevelDef
        skipWhitespace >> P.eof
        exportOperators exports -- at the end we update the operator table
        return $ Module (ModuleId [] name) path imports exports definitions
  where
    -- Given the import information locate and parse a submodule.
    importFile :: Located RawImport -> ParserIO (Import ())
    importFile (Located position (id, alias, identifiers)) = do
        let ModuleId prefix mod = id
        basePath <- getCurrentPath
        path <- findModulePath basePath prefix mod
        mod <- file path
        -- TODO: extract imported operators and put them in the current map
        return $ Import mod id alias position identifiers
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
    exportOperators Nothing = clearLocalOperators
    exportOperators (Just exports) = do
        state <- lift get
        let locOpInfo = stateCurrentOps state
            locOpList = foldr (++) [] locOpInfo
            globalOps = stateExportedOps state
        insertOpExports locOpInfo locOpList exports' globalOps []
        clearLocalOperators
      where
        -- T.Text identifiers from exports list
        exports' = filter isOp $ concatMap (stripExport . unlocated) exports
        -- Checks if something is an operator by checking the first character.
        isOp :: T.Text -> Bool
        isOp name = T.head name `elem` (':' : operatorCharsList)
        -- Convert a ImportedValue wrapper into Text identifier(s).
        stripExport :: ImportedValue -> [T.Text]
        stripExport (ImportedIdentifier (Identifier id)) = [id]
        stripExport (ImportedType _ Nothing) = []
        stripExport (ImportedType _ (Just constructors)) = 
            map (\(ConstructorName name) -> name) constructors
        -- Actually inserts local operator definitions into the parser state.
        insertOpExports :: LocalOperators
                        -> [T.Text]
                        -> [T.Text]
                        -> GlobalOperators 
                        -> [(T.Text, Priority, Fixity)]
                        -> ParserIO ()
        insertOpExports _ _ [] globals locals = do
            path <- getCurrentPath
            let newGlobals = Map.insert path locals globals
            state <- lift get
            lift $ put state { stateExportedOps = newGlobals }
        insertOpExports opInfo opList (e:es) globals locals
            | e `elem` opList = let info = findOpInfo e opInfo in
                insertOpExports opInfo opList es globals (info : locals)
            | otherwise = fail $ "Undefined exported operator " ++ show e
        -- Local operator table has fixity and priority as keys for parsing
        -- performance. This function inverts the map to find the fixity
        -- and priority for a given operator. It is assumed, that the operator
        -- is defined.
        findOpInfo :: T.Text -> LocalOperators -> (T.Text, Priority, Fixity)
        findOpInfo op operators = (\(p,f) -> (op, p, f)) . fst . fromJust $ 
            find (elem op . snd) (Map.toList operators)
    -- Remove the local operator table from the parser state.
    clearLocalOperators :: ParserIO ()
    clearLocalOperators = do
        state <- lift get
        lift $ put state {stateCurrentOps = Map.empty}

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
        Nothing -> fail $ "cannot find module " ++ show (unwrapName name)
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
    name <- qualifiedModuleName <?> "module name"
    list <- moduleIdentifierList
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
    name <- uppercaseName <?> "module name"
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

-- Parser for top-level definitions (types, classes, instances, let, etc.)
topLevelDef :: ParserIO (TopLevelDef a)
topLevelDef = do
    P.optional . void . P.try $ operatorDecl
    return undefined

-- Parser for operator declaration. Every declaration must be followed
-- by the operator definition (either a let or type definition).
operatorDecl :: ParserIO ()
operatorDecl = do
    keyword "operator"
    op <- infixIdentifier
    fixity <- operatorFixity <?> "operator fixity (left, right or none)"
    priority <- L.decimal <?> "operator precedence (1..10)"
    checkPriority priority
    assertUndefined op
-- TODO: during the import parsing store which operators are visible
--       in what files
    return undefined
  where
    -- Assert that the priority is from the range 1..10
    checkPriority :: Integer -> ParserIO ()
    checkPriority p = when (p < 1 || p > 10) $
        fail $ "precedence must be in range (1..10), got \"" ++ show p ++ "\""
    -- Assert that we are not overwritting some other operator.
    assertUndefined :: T.Text -> ParserIO ()
    assertUndefined = undefined
        
-- Parser for operator fixity in operator declarations.
operatorFixity :: ParserIO Fixity
operatorFixity = (keyword "left" $> LeftFix) 
            <|> (keyword "right" $> RightFix) 
            <|> (keyword "none" $> NoneFix)

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
separatedList start end elem sep = start' *> (elements <|> pure []) <* end'
  where
    start' = symbol start
    end'   = symbol end
    elements = pure (:) <*> elem <*> P.many (symbol sep *> elem)

-- Parser for import lists. Despite the name it is also used to parse
-- the export lists. The lists are optional so it returns Maybe.
moduleIdentifierList :: ParserIO (Maybe [Located ImportedValue])
moduleIdentifierList = P.optional list
  where
    list = separatedList "{" "}" (located exportElem) ","
    -- Parser for single imported/exported thing (identifier or a type name
    -- (possibly with constructors)).
    exportElem :: ParserIO ImportedValue
    exportElem = (ImportedIdentifier . Identifier <$> importIdentifier)
             <|> (uncurry ImportedType <$> typeImport)
    -- Parser for identifiers that can be imported/exported 
    -- (normal operators in brackets or variable names)
    importIdentifier :: ParserIO T.Text
    importIdentifier = 
        lowercaseName <|> (surroundedBy "(" operator ")") <?> "identifier"
    -- Parser for type imports. Type imports consist of type name optionally
    -- followed by the explicit type constructor list. [] is a valid type name.
    typeImport :: ParserIO (TypeName, Maybe [ConstructorName])
    typeImport = do
        typeName <- TypeName <$> typeName <?> "type name"
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
surroundedBy :: T.Text -> ParserIO T.Text -> T.Text -> ParserIO T.Text
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

-- Parser for primitive expressions like integers, booleans, variables.
primExpr :: ParserIO (Expr ())
primExpr = (P.try float <?> "float literal")
    <|> character
    <|> (integer <?> "integer literal")
    <|> string
    <|> (P.try boolean <?> "bool literal")
    <|> (P.try varExpr <?> "identifier")
    <|> (P.try constrExpr <?> "constructor")
    -- TODO: qualified name
    <|> (unit <?> "()")

-- Parser for variable names. Only lowercase identifiers and operators within
-- parenthesis are valid names.
varExpr :: ParserIO (Expr ())
varExpr = withPos $ \pos -> do
    var <- lowercaseName 
        <|> surroundedBy "(" operator ")"
    return $ Var (Identifier var) pos ()

-- Parser for constructor names as primitive expressions.
constrExpr :: ParserIO (Expr ())
constrExpr = withPos $ \pos -> do
    constr <- constructorName
    return $ Constructor constr pos ()

-- Parser for floating point numbers.
float :: ParserIO (Expr ())
float = withPos $ \pos -> do
    num <- L.signed (pure ()) L.float
    return $ Primitive (FloatLit num) pos ()

-- Parser for integers. Decimal, octal, hex and binary literals are supported.
-- The numbers must be in range [-2^63 .. 2^63 - 1] or else the parsing fails.
integer :: ParserIO (Expr ())
integer = withPos $ \pos -> do
    -- TODO: check for overflows
    num <- (L.signed (pure ()) parseInt)
    checkOverflow num
    return $ Primitive (IntLit $ fromInteger num) pos ()
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
string :: ParserIO (Expr ())
string = withPos $ \pos -> do
    str <- P.char '"' *> (T.pack <$> P.manyTill L.charLiteral (P.char '"'))
    return $ Primitive (StringLit str) pos ()

-- Parser for character literals.
character :: ParserIO (Expr ())
character = withPos $ \pos -> do
    c <- P.char '\'' *> L.charLiteral <* P.char '\''
    return $ Primitive (CharLit c) pos ()

-- Parser for boolean literals ('True' or 'False')
boolean :: ParserIO (Expr ())
boolean = withPos $ \pos -> do
    val <- (keyword "True" $> True) <|> (keyword "False" $> False)
    return $ Primitive (BoolLit val) pos ()

-- Parser for units. Parens may be separated by whitespace.
unit :: ParserIO (Expr ())
unit = withPos $ \pos -> do
    symbol "("
    symbol ")"
    return $ Primitive UnitLit pos ()

-- Parser for a single character that is allowed in identifiers.
nameChar :: ParserIO Char
nameChar = P.choice [P.alphaNumChar, P.char '\'', P.char '_']

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
    opts <- getOptions
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
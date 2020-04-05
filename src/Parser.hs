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
import System.FilePath.Posix (takeDirectory, (</>), (<.>))
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, forM, when)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Void (Void)
import Utility (Position(..), findM)
import GetOpt (Options(..), ModulePath(..), getOptions)
import qualified Data.Map as Map
import Syntax as AST

type GetOptIO = ReaderT Options IO
type StateIO  = StateT ParserState GetOptIO
type ParserIO = P.ParsecT Void T.Text StateIO

type Errors   = P.ParseErrorBundle T.Text Void

data ParserState = ParserState
    { stateOperators   :: Map.Map Priority OperatorTable
    , stateVisited     :: Map.Map FilePath (Module ())
    , stateModuleStack :: [(ModName, FilePath)]
    }
  deriving Show

type Priority = Int

newtype ModuleHeader = ModuleHeader
    { moduleName :: AST.ModuleId
    }
  deriving Show

data CustomOp = CustomOp
    { opSymbol :: T.Text
    , opOrigin :: AST.ModuleId
    }
  deriving Show

data OperatorTable = OpTable
    { opsLeft  :: [CustomOp]
    , opsRight :: [CustomOp]
    , opsNone  :: [CustomOp]
    }
  deriving Show

type RawImport = (ModuleId, Maybe ModName, Maybe [Located ImportedValue])


-- TODO: decide what the path should be
stdLibraryDir :: FilePath
stdLibraryDir = undefined

fileExtension :: String
fileExtension = ".zo"

preludeName :: FilePath
preludeName = "Core" 

stdPreludePath :: FilePath
stdPreludePath = stdLibraryDir </> preludeName <.> fileExtension

newParserState :: ParserState
newParserState = ParserState Map.empty Map.empty []

program :: ParserIO (AST.Program ())
program = do
    opts <- show <$> getopt
    -- TODO: import prelude
    rootFile <- head . optInputs <$> getopt
    Program <$> file rootFile

file :: FilePath -> ParserIO (Module ())
file path = do
    visited <- stateVisited <$> lift get
    case Map.lookup path visited of
        Nothing -> do
            fileContents <- liftIO . T.readFile $ path
            result <- lift $ P.runParserT (module' path) path fileContents
            handleError result
        Just mod -> return mod
  where
    handleError :: Either Errors (Module ()) -> ParserIO (Module ())
    handleError (Right mod) = return mod
    handleError (Left err) = 
        liftIO $ putStrLn (P.errorBundlePretty err) >> exitFailure

module' :: FilePath -> ParserIO (Module())
module' path = do
    skipWhitespace
    (name, exports) <- moduleHeader <?> "module header"
    pushModulePath name path
    rawImports <- P.many $ located (import' <?> "module import")
    imports <- forM rawImports importFile
    definitions <- P.many $ topLevelDef
    P.eof
    return $ Module (ModuleId [] name) path imports exports definitions
  where
    importFile :: Located RawImport -> ParserIO (Import ())
    importFile (Located position (id, alias, identifiers)) = do
        let ModuleId prefix mod = id
        basePath <- getCurrentPath
        path <- findModulePath basePath prefix mod
        mod <- file path
        checkModuleName $ AST.moduleName . moduleId $ mod
        return $ Import mod id alias position identifiers
      where
        checkModuleName :: ModName -> ParserIO ()
        checkModuleName name
            | AST.moduleName id /= name = fail $ 
                "Module name " ++ show name ++ " doesn't match the file name!"
            | otherwise = return ()

findModulePath :: FilePath -> [ModName] -> ModName -> ParserIO FilePath
findModulePath current prefix name = do
    externalModules <- optModules <$> getopt
    let choices = absPath : (optModPath <$> filter matching externalModules)
    path <- findM (liftIO . doesFileExist) choices
    case path of
        Nothing -> fail $ "Cannot find module " ++ show (unwrapName name)
        Just path -> return path
  where
    absPath = current' </> relativePath
    current' = takeDirectory current
    relativePath = foldPath prefix name
    foldPath :: [ModName] -> ModName -> FilePath
    foldPath prefix (ModName name) = 
        foldl joinPath "" prefix </> (T.unpack name) <.> fileExtension
      where
        joinPath path (ModName dir) = path </> (T.unpack dir)
    matching :: ModulePath -> Bool
    matching mod
        | null prefix = optModName mod == unwrapName name
        | otherwise   = optModName mod == unwrapName (head prefix)
    unwrapName (ModName name) = name

import' :: ParserIO RawImport
import' = do
    keyword "import"
    name <- qualifiedModuleName <?> "module name"
    list <- moduleIdentifierList
    alias <- P.optional $ keyword "as" *> (uppercaseName <?> "name alias")
    return (name, ModName <$> alias, list)

getCurrentPath :: ParserIO FilePath
getCurrentPath = do
    state <- lift get
    return $ snd . head . stateModuleStack $ state

qualifiedModuleName :: ParserIO ModuleId
qualifiedModuleName = do
    prefix <- P.many $ P.try $ (uppercaseName) <* sep
    name   <- uppercaseName
    return $ ModuleId (ModName <$> prefix) (ModName name) 
  where
    sep = P.char '\\' <?> "scope operator \"\\\""

moduleHeader :: ParserIO (ModName, Maybe [Located ImportedValue])
moduleHeader = do
    keyword "module"
    name <- uppercaseName <?> "module name"
    exports <- moduleIdentifierList
    return (ModName name, exports)

pushModulePath :: ModName -> FilePath -> ParserIO ()
pushModulePath name path = do
    state <- lift get
    lift $ put state { stateModuleStack = (name, path) : stateModuleStack state}

topLevelDef :: ParserIO (TopLevelDef a)
topLevelDef = undefined

reserved :: [T.Text]
reserved = ["module", "import", "class", "instance", "let", "in", "with",
            "match", "case", "and", "or", "fn", "type", "alias",
            "end", "if", "then", "else", "_external", "_internal"]

upperReserved :: [T.Text]
upperReserved = ["True", "False"]

reservedOperators :: [T.Text]
reservedOperators = [":", "=>", "->", "@", "=", ":="]

uppercaseName :: ParserIO T.Text
uppercaseName = lexeme $ do
    name <- T.pack <$> (pure (:) <*> P.upperChar <*> P.many nameChar)
    when (name `elem` upperReserved) $
        fail ("Keyword " ++ show name ++ " is not a valid identifier!")
    return name

lowercaseName :: ParserIO T.Text 
lowercaseName = lexeme $ do
    name <- T.pack <$> (pure (:) <*> P.lowerChar <*> P.many nameChar)
    when (name `elem` reserved) $
        fail ("Keyword " ++ show name ++ " is not a valid identifier!")
    return name

constructorOperator :: ParserIO T.Text
constructorOperator = lexeme $ do
    op <- T.pack <$> (pure (:) <*> P.char ':' <*> P.some operatorChar)
    when (op `elem` reservedOperators) $
        fail ("Operator " ++ show op ++ " is a reserved operator!")
    return op

operator :: ParserIO T.Text
operator = lexeme $ do
    op <- T.pack <$> P.some operatorChar
    when (op `elem` reservedOperators) $
        (fail $ "Operator " ++ show op ++ " is a reserved operator!")
    return op

operatorChar :: ParserIO Char
operatorChar = P.oneOf ['=', '+', '-', '*', '.', '/', '!', '~', '^',
                        '$', '%', '&', '?', '>', '<', ':', '@', '|']

located :: ParserIO a -> ParserIO (Located a)
located parser = withPos $ \pos -> Located pos <$> parser

separatedList :: T.Text -> T.Text -> ParserIO a -> T.Text -> ParserIO [a]
separatedList start end elem sep = start' *> (elements <|> pure []) <* end'
  where
    start' = symbol start
    end'   = symbol end
    elements = pure (:) <*> elem <*> P.many (symbol sep *> elem)

moduleIdentifierList :: ParserIO (Maybe [Located ImportedValue])
moduleIdentifierList = P.optional list
  where
    list = separatedList "{" "}" (located exportElem) ","
    exportElem :: ParserIO ImportedValue
    exportElem = (ImportedIdentifier . Identifier <$> importIdentifier)
             <|> (uncurry ImportedType <$> typeImport)
    importIdentifier :: ParserIO T.Text
    importIdentifier = 
        lowercaseName <|> (surroundedBy "(" operator ")") <?> "identifier"
    typeImport :: ParserIO (TypeName, Maybe [ConstructorName])
    typeImport = do
        typeName <- TypeName <$> uppercaseName <?> "type name"
        constructors <- P.optional constructorList
        return (typeName, constructors)
    constructorList :: ParserIO [ConstructorName]
    constructorList = separatedList "{" "}" constructorName "," 
        <?> "constructor list"

surroundedBy :: T.Text -> ParserIO T.Text -> T.Text -> ParserIO T.Text
surroundedBy left parser right = symbol left *> parser <* symbol right

constructorName :: ParserIO ConstructorName
constructorName = ConstructorName <$> 
    (uppercaseName <|> surroundedBy "(" constructorOperator ")")

keyword :: T.Text -> ParserIO ()
keyword kw = keywordParserIO <?> ("keyword " ++ show kw)
  where
    keywordParserIO = (lexeme . P.try) $ P.string kw *> P.notFollowedBy nameChar

primExpr :: ParserIO (Expr ())
primExpr = (P.try float <?> "float literal")
    <|> character
    <|> (integer <?> "integer literal")
    <|> string
    <|> (P.try boolean <?> "bool literal")
    <|> (variable <?> "identifier")
    <|> (unit <?> "()")

variable :: ParserIO (Expr ())
variable = withPos $ \pos -> do
    var <- lowercaseName 
        <|> surroundedBy "(" operator ")"
        <|> uppercaseName
        <|> surroundedBy "(" constructorOperator ")"
    return $ Var (Identifier var) pos ()

float :: ParserIO (Expr ())
float = withPos $ \pos -> do
    num <- L.signed (pure ()) L.float
    return $ Primitive (FloatLit num) pos ()

integer :: ParserIO (Expr ())
integer = withPos $ \pos -> do
    -- TODO: check for overflows
    num <- (L.signed (pure ()) parseInt)
    checkOverflow num
    return $ Primitive (IntLit $ fromInteger num) pos ()
  where
    parseInt :: ParserIO Integer
    parseInt = (P.string "0o" >> L.octal)
        <|> (P.string "0x" >> L.hexadecimal)
        <|> (P.string "0b" >> L.binary)
        <|> L.decimal
    checkOverflow :: Integer -> ParserIO ()
    checkOverflow i = when (i <  min || i > max) $ fail (overflowError i)
      where
        min = toInteger (minBound :: Int)
        max = toInteger (maxBound :: Int)
        overflowError int = 
            "overflowing integer literal '" ++ show int ++ "'"

string :: ParserIO (Expr ())
string = withPos $ \pos -> do
    str <- P.char '"' *> (T.pack <$> P.manyTill L.charLiteral (P.char '"'))
    return $ Primitive (StringLit str) pos ()

character :: ParserIO (Expr ())
character = withPos $ \pos -> do
    c <- P.char '\'' *> L.charLiteral <* P.char '\''
    return $ Primitive (CharLit c) pos ()

boolean :: ParserIO (Expr ())
boolean = withPos $ \pos -> do
    val <- (keyword "True" $> True) <|> (keyword "False" $> False)
    return $ Primitive (BoolLit val) pos ()

unit :: ParserIO (Expr ())
unit = withPos $ \pos -> do
    symbol "("
    symbol ")"
    return $ Primitive UnitLit pos ()

nameChar :: ParserIO Char
nameChar = P.choice [P.alphaNumChar, P.char '\'', P.char '_']

withPos :: P.MonadParsec e s m => (Position -> m a) -> m a
withPos f = do
    state  <- P.getParserState
    offset <- P.getOffset
    let file = P.sourceName . P.pstateSourcePos . P.statePosState $ state
    f $ Position offset file 

skipWhitespace :: ParserIO ()
skipWhitespace = L.space (void P.spaceChar) lineComment blockComment
  where
    lineComment  = P.try (P.char '#' *> P.notFollowedBy ":") >> skipLine
    blockComment = L.skipBlockCommentNested "#:" ":#"
    skipLine :: ParserIO ()
    skipLine = void $ P.takeWhileP (Just "character") (/= '\n')

lexeme :: ParserIO a -> ParserIO a
lexeme = L.lexeme skipWhitespace

symbol :: T.Text -> ParserIO T.Text
symbol = L.symbol skipWhitespace

getopt :: ParserIO Options
getopt = lift . lift $ ask

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

parseProgram :: IO (Program ())
parseProgram = do
    opts <- getOptions
    result <- runParser program "" "" newParserState opts
    case result of
        Left err  -> do 
            putStrLn $ P.errorBundlePretty err
            exitFailure
        Right program -> return program

testParser :: Show a => ParserIO a -> T.Text -> IO ()
testParser parser input = do
    result <- parsingResult
    case result of
        Left err     -> putStrLn $ P.errorBundlePretty err
        Right result -> putStrLn $ show result
  where
    emptyOpts = Options "" [] []
    parsingResult = runParser parser "" input newParserState emptyOpts
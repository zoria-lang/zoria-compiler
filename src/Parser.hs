{-# LANGUAGE OverloadedStrings #-}

module Parser (Parser, runParser, newParserState) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec ((<?>))
import System.FilePath.Posix ((</>))
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, forM, when)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Void (Void)
import Utility (Position(..))
import GetOpt (Options(..))
import Data.Map as Map
import Data.Set as Set
import Syntax as AST


type GetOptIO = ReaderT Options IO
type StateIO  = StateT ParserState GetOptIO
type Parser   = P.ParsecT Void T.Text StateIO

type Errors   = P.ParseErrorBundle T.Text Void

data ParserState = ParserState
    { stateOperators :: Map.Map Priority OperatorTable
    , stateVisited   :: Set.Set ModuleId
    , stateModule    :: [ModuleHeader]
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

-- TODO: decide what the path should be
stdLibraryDir :: FilePath
stdLibraryDir = undefined

preludeName :: FilePath
preludeName = "Core.zo"

stdPreludePath :: FilePath
stdPreludePath = stdLibraryDir </> preludeName

newParserState :: ParserState
newParserState = ParserState Map.empty Set.empty []

program :: Parser (AST.Program ())
program = do
    -- at the moment the rest of the input files is discarded
    -- TODO: assure that there can be only one input file specified
    rootFile <- head . optInputs <$> getopt
    rootModule <- moduleParser rootFile
    return $ Program rootModule

moduleParser :: FilePath -> Parser (Module ())
moduleParser path = withFreshState $ do
    -- opening a file doesn't do anything. We need to swap the inputs somehow.
    fileContents <- liftIO $ T.readFile path
    setFreshState path fileContents
    protoHeader <- (moduleHeader <?> "module header")
    -- parse the imports
    -- parse the file
    return undefined
  where
    setFreshState :: FilePath -> T.Text -> Parser ()
    setFreshState path input = do
        state <- P.getParserState
        let sourcePos   = P.SourcePos path (P.mkPos 1) (P.mkPos 1)
            oldPosState = P.statePosState state
            tabWidth    = P.pstateTabWidth oldPosState
            linePrefix  = P.pstateLinePrefix oldPosState
            errors      = P.stateParseErrors state
            posState    = P.PosState input 0 sourcePos tabWidth linePrefix
        P.setParserState (P.State input 0 posState errors)

withFreshState :: Parser a -> Parser a
withFreshState parser = do
    oldState <- P.getParserState
    parsingResult <- parser
    P.setParserState oldState
    return parsingResult

moduleHeader :: Parser (Module ())
moduleHeader = do
    keyword "module"
    name <- uppercaseName
    exports <- moduleIdentifierList
    return undefined

reserved :: [T.Text]
reserved = ["module", "import", "class", "instance", "let", "in", "with",
            "match", "case", "and", "or", "fn", "type", "alias",
            "end", "if", "then", "else", "_external", "_internal"]

upperReserved :: [T.Text]
upperReserved = ["True", "False"]

reservedOperators :: [T.Text]
reservedOperators = [":", "=>", "->", "@", "=", ":="]

uppercaseName :: Parser T.Text
uppercaseName = lexeme $ do
    name <- T.pack <$> (pure (:) <*> P.upperChar <*> P.many nameChar)
    when (name `elem` upperReserved) $
        fail ("Keyword " ++ show name ++ " is not a valid identifier!")
    return name

lowercaseName :: Parser T.Text 
lowercaseName = lexeme $ do
    name <- T.pack <$> (pure (:) <*> P.lowerChar <*> P.many nameChar)
    when (name `elem` reserved) $
        fail ("Keyword " ++ show name ++ " is not a valid identifier!")
    return name

constructorOperator :: Parser T.Text
constructorOperator = lexeme $ do
    op <- T.pack <$> (pure (:) <*> P.char ':' <*> P.some operatorChar)
    when (op `elem` reservedOperators) $
        fail ("Operator " ++ show op ++ " is a reserved operator!")
    return op

operator :: Parser T.Text
operator = lexeme $ do
    op <- T.pack <$> P.some operatorChar
    when (op `elem` reservedOperators) $
        (fail $ "Operator " ++ show op ++ " is a reserved operator!")
    return op

operatorChar :: Parser Char
operatorChar = P.oneOf ['=', '+', '-', '*', '.', '/', '!', '~', 
                        '$', '%', '&', '?', '>', '<', ':', '@']

located :: Parser a -> Parser (Located a)
located parser = withPos $ \pos -> Located pos <$> parser

separatedList :: T.Text -> T.Text -> Parser a -> T.Text -> Parser [a]
separatedList start end elem sep = start' *> (elements <|> pure []) <* end'
  where
    start' = symbol start
    end'   = symbol end
    elements = pure (:) <*> elem <*> P.many (symbol sep *> elem)

moduleIdentifierList :: Parser (Maybe [Located ImportedValue])
moduleIdentifierList = P.optional list
  where
    list = separatedList "{" "}" (located exportElem) ","
    exportElem :: Parser ImportedValue
    exportElem = (ImportedIdentifier . Identifier <$> importIdentifier)
             <|> (uncurry ImportedType <$> typeImport)
    importIdentifier :: Parser T.Text
    importIdentifier = 
        lowercaseName <|> (surroundedBy "(" operator ")") <?> "identifier"
    typeImport :: Parser (TypeName, Maybe [ConstructorName])
    typeImport = do
        typeName <- TypeName <$> uppercaseName <?> "type name"
        constructors <- P.optional constructorList
        return (typeName, constructors)
    constructorList :: Parser [ConstructorName]
    constructorList = separatedList "{" "}" constructorName "," 
        <?> "constructor list"

surroundedBy :: T.Text -> Parser T.Text -> T.Text -> Parser T.Text
surroundedBy left parser right = symbol left *> parser <* symbol right

constructorName :: Parser ConstructorName
constructorName = ConstructorName <$> 
    (uppercaseName <|> surroundedBy "(" constructorOperator ")")

keyword :: T.Text -> Parser ()
keyword kw = keywordParser <?> ("keyword " ++ show kw ++ "'")
  where
    keywordParser = (lexeme . P.try) $ P.string kw *> P.notFollowedBy nameChar

primExpr :: Parser (Expr ())
primExpr = (P.try float <?> "float literal")
    <|> character
    <|> (integer <?> "integer literal")
    <|> string
    <|> (P.try boolean <?> "bool literal")
    <|> (P.try variable <?> "identifier")
    <|> (unit <?> "()")

variable :: Parser (Expr ())
variable = withPos $ \pos -> do
    var <- lowercaseName 
        <|> surroundedBy "(" operator ")"
        <|> uppercaseName
        <|> surroundedBy "(" constructorOperator ")"
    return $ Var (Identifier var) pos ()

float :: Parser (Expr ())
float = withPos $ \pos -> do
    num <- L.signed (pure ()) L.float
    return $ Primitive (FloatLit num) pos ()

integer :: Parser (Expr ())
integer = withPos $ \pos -> do
    -- TODO: check for overflows
    num <- (L.signed (pure ()) parseInt)
    checkOverflow num
    return $ Primitive (IntLit $ fromInteger num) pos ()
  where
    parseInt :: Parser Integer
    parseInt = (P.string "0o" >> L.octal)
        <|> (P.string "0x" >> L.hexadecimal)
        <|> (P.string "0b" >> L.binary)
        <|> L.decimal
    checkOverflow :: Integer -> Parser ()
    checkOverflow i = when (i <  min || i > max) $ fail (overflowError i)
      where
        min = toInteger (minBound :: Int)
        max = toInteger (maxBound :: Int)
        overflowError int = 
            "overflowing integer literal '" ++ show int ++ "'"

string :: Parser (Expr ())
string = withPos $ \pos -> do
    str <- P.char '"' *> (T.pack <$> P.manyTill L.charLiteral (P.char '"'))
    return $ Primitive (StringLit str) pos ()

character :: Parser (Expr ())
character = withPos $ \pos -> do
    c <- P.char '\'' *> L.charLiteral <* P.char '\''
    return $ Primitive (CharLit c) pos ()

boolean :: Parser (Expr ())
boolean = withPos $ \pos -> do
    val <- (keyword "True" $> True) <|> (keyword "False" $> False)
    return $ Primitive (BoolLit val) pos ()

unit :: Parser (Expr ())
unit = withPos $ \pos -> do
    symbol "("
    symbol ")"
    return $ Primitive UnitLit pos ()

nameChar :: Parser Char
nameChar = P.choice [P.alphaNumChar, P.char '\'', P.char '_']

withPos :: P.MonadParsec e s m => (Position -> m a) -> m a
withPos f = do
    state  <- P.getParserState
    offset <- P.getOffset
    let file = P.sourceName . P.pstateSourcePos . P.statePosState $ state
    f $ Position offset file 

skipWhitespace :: Parser ()
skipWhitespace = L.space (void P.spaceChar) lineComment blockComment
  where
    lineComment  = L.skipLineComment "#"
    blockComment = L.skipBlockComment "#:" ":#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipWhitespace

symbol :: T.Text -> Parser T.Text
symbol = L.symbol skipWhitespace

getopt :: Parser Options
getopt = lift . lift $ ask

runParser :: Parser a 
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

testParser :: Show a => Parser a -> T.Text -> IO ()
testParser parser input = do
    result <- parsingResult
    case result of
        Left err     -> putStrLn $ P.errorBundlePretty err
        Right result -> putStrLn $ show result
  where
    emptyOpts = Options "" [] []
    parsingResult = runParser parser "" input newParserState emptyOpts
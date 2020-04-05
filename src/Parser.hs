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
import Control.Monad (void, forM)
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
stdLibraryPath :: FilePath
stdLibraryPath = undefined

preludeName :: FilePath
preludeName = "Core.zo"

stdPreludePath :: FilePath
stdPreludePath = stdLibraryPath </> preludeName

newParserState :: ParserState
newParserState = ParserState Map.empty Set.empty []

program :: Parser (AST.Program ())
program = do
    -- at the moment the rest of the input files is discarded
    -- TODO: assure that there can be only one input file specified
    rootFile <- head . optInputs <$> getopt
    rootModule <- parseFile rootFile
    return $ Program rootModule

parseFile :: FilePath -> Parser (Module ())
parseFile path = do
    file <- liftIO $ readFile path
    protoHeader <- moduleHeader
    return undefined

moduleHeader :: Parser (Module ())
moduleHeader = do
    keyword "module"
    name <- uppercaseName
    exports <- moduleIdentifierList
    return undefined

uppercaseName :: Parser T.Text
uppercaseName = lexeme $ do
    first <- P.upperChar
    rest  <- P.some nameChar
    return . T.pack $ first : rest

lowercaseName :: Parser T.Text 
lowercaseName = lexeme $ do
    first <- P.lowerChar
    rest  <- P.some nameChar
    return . T.pack $ first : rest

located :: Parser a -> Parser (Located a)
located parser = withPos $ \pos -> Located pos <$> parser

separatedList :: T.Text -> T.Text -> Parser a -> T.Text -> Parser [a]
separatedList start end elem sep = start' *> (elements <|> pure []) <* end' 
  where
    start' = symbol start
    end'   = symbol end
    elements = pure (:) <*> elem <*> P.many (symbol sep *> elem)

moduleIdentifierList :: Parser (Maybe [Located ImportedValue])
moduleIdentifierList = Just <$> list <|> pure Nothing
  where
    list = separatedList "{" "}" exportElem ","
    exportElem :: Parser (Located ImportedValue)
    exportElem = undefined

keyword :: T.Text -> Parser ()
keyword kw = keywordParser <?> T.unpack kw
  where
    keywordParser = (lexeme . P.try) $ P.string kw *> P.notFollowedBy nameChar

primExpr :: Parser (Expr ())
primExpr = (P.try float <?> "float")
    <|> character
    <|> (integer <?> "integer")
    <|> string
    <|> boolean
    <|> (unit <?> "()")

float :: Parser (Expr ())
float = withPos $ \pos -> do
    num <- L.signed (pure ()) L.float
    return $ Primitive (FloatLit num) pos ()

integer :: Parser (Expr ())
integer = withPos $ \pos -> do
    -- TODO: check for overflows
    num <- (L.signed (pure ()) parseInt)
    return $ Primitive (IntLit num) pos ()
  where
    parseInt :: Parser Int
    parseInt = (P.string "0o" >> L.octal)
        <|> (P.string "0x" >> L.hexadecimal)
        <|> (P.string "0b" >> L.binary)
        <|> L.decimal

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
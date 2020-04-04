{-# LANGUAGE OverloadedStrings #-}

module Parser 
    (Parser, runParser, test) 
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Void (Void)
import Control.Monad (void, mapM_)
import Utility (Position(..))
import GetOpt (Options(..), ModulePath(..))


type ParserState = Int -- TODO: change

type GetOptIO = ReaderT Options IO
type StateIO  = StateT ParserState GetOptIO
type Parser = P.ParsecT Void T.Text StateIO
type Errors = P.ParseErrorBundle T.Text Void


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

symbol' :: T.Text -> Parser (Position, T.Text)
symbol' x = withPos $ \pos -> do
    symbol <- symbol x
    return (pos, symbol)

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

-- example:
test :: Parser ()
test = withPos $ \pos -> do
    liftIO $ putStrLn $ show pos
    len <- length <$> P.many "*"
    opts <- getopt
    liftIO $ putStrLn $ show opts
    st <- lift get
    liftIO $ putStrLn $ "Old value: " ++ show st ++ "\n"
    lift $ put (st + len)
    st' <- lift get
    liftIO $ putStrLn $ "New value: " ++ show st' ++ "\n"
    return ()

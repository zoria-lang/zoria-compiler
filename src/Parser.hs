{-# LANGUAGE OverloadedStrings #-}

module Parser 
    () 
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Void (Void)
import Control.Monad (void, mapM_)
import Utility (Position(..))


type ParserState = Int -- TODO: change
type IOState = StateT ParserState IO
type Parser = P.ParsecT Void T.Text IOState
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

liftState :: IOState a -> Parser a
liftState = lift

liftIO :: IO a -> Parser a
liftIO = lift . lift

runParser :: Parser a 
          -> FilePath 
          -> T.Text 
          -> ParserState 
          -> IO (Either Errors a)
runParser parser file input initState = do
    (result, _) <- runStateT state initState
    return result
  where
    state = P.runParserT parser file input

-- example:
test :: Parser ()
test = withPos $ \pos -> do
    liftIO $ putStrLn $ show pos
    len <- length <$> P.many "*"
    st <- liftState get
    liftIO $ putStrLn $ "Old value: " ++ show st ++ "\n"
    liftState $ put (st + len)
    st' <- liftState get
    liftIO $ putStrLn $ "New value: " ++ show st' ++ "\n"
    return ()

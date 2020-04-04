{-# LANGUAGE OverloadedStrings #-}

module Parser 
    () 
where

import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad (void)


type IOParser = P.ParsecT Void T.Text IO

type Parser = P.Parsec Void T.Text

data Position = Position
    { posOffset :: !Int
    , posFile   :: FilePath
    }
  deriving Show


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

test :: Parser Position
test = withPos return
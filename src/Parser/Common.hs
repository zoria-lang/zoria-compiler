{-# LANGUAGE OverloadedStrings #-}
module Parser.Common where

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           Data.Functor                   ( ($>) )
import           Text.Megaparsec                ( (<?>) )

type Parser = P.Parsec Void T.Text
type Errors = P.ParseErrorBundle T.Text Void

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

symbol :: T.Text -> Parser T.Text
symbol = L.symbol whitespace

keyword :: T.Text -> Parser ()
keyword kw = kwKeyword <?> ("keyword '" ++ T.unpack kw ++ "'")
  where
    kwKeyword = lexeme (symbol kw <* P.notFollowedBy P.alphaNumChar) $> ()

untilEof :: Parser a -> Parser a
untilEof = (<* P.eof)

whitespace :: Parser ()
whitespace = L.space P.space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "#"
    blockComment = L.skipBlockCommentNested "{#" "#}"

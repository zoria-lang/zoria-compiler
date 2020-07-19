{-# LANGUAGE OverloadedStrings #-}
module Parser.Common where

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L
import qualified Data.Text                     as T
import           Data.Void                      ( Void )

type Parser = P.Parsec Void T.Text
type Errors = P.ParseErrorBundle T.Text Void

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

symbol :: T.Text -> Parser T.Text
symbol = L.symbol whitespace

list :: Parser start -> Parser a -> Parser end -> Parser sep -> Parser [a]
list start elem end sep = P.between start end (P.sepBy elem sep)

list' :: T.Text -> Parser a -> T.Text -> T.Text -> Parser [a]
list' start elem end sep = list (symbol start) elem (symbol end) (symbol sep)

list1 :: Parser start -> Parser a -> Parser end -> Parser sep -> Parser [a]
list1 start elem end sep = P.between start end (P.sepBy1 elem sep)

list1' :: T.Text -> Parser a -> T.Text -> T.Text -> Parser [a]
list1' start elem end sep = list1 (symbol start) elem (symbol end) (symbol sep)

whitespace :: Parser ()
whitespace = L.space P.space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "#"
    blockComment = L.skipBlockCommentNested "{#" "#}"


{-# LANGUAGE OverloadedStrings #-}
module Parser.Primitive where

import Parser.ParserIO
import Parser.Common
import Parser.Identifier
import Syntax
import Control.Applicative ((<|>))
import Control.Monad (when)
import Text.Megaparsec ((<?>))
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L


-- Parser for literal expressions.
primExpr :: ParserIO PrimExpr
primExpr = lexeme $ (P.try float <?> "float literal")
    <|> (character <?> "character")
    <|> (integer <?> "integer literal")
    <|> (StringLit <$> string <?> "string literal")
    <|> P.try boolean
    <|> (unit <?> "()")

-- Parser for floating point numbers.
float :: ParserIO PrimExpr
float = FloatLit <$> L.signed (pure ()) L.float

-- Parser for integers. Decimal, octal, hex and binary literals are supported.
-- The numbers must be in range [-2^63 .. 2^63 - 1] or else the parsing fails.
integer :: ParserIO PrimExpr
integer = do
    num <- L.signed (pure ()) parseInt
    checkOverflow num
    return . IntLit . fromInteger $ num
  where
    -- Parser for a integer literal.
    parseInt :: ParserIO Integer
    parseInt = P.try (P.string "0o" >> L.octal)
           <|> P.try (P.string "0x" >> L.hexadecimal)
           <|> P.try (P.string "0b" >> L.binary)
           <|> L.decimal
    -- Function that checks whether the literal is within the correct range.
    checkOverflow :: Integer -> ParserIO ()
    checkOverflow i = when (i <  min || i > max) $ fail (overflowError i)
      where
        min = toInteger (minBound :: Int)
        max = toInteger (maxBound :: Int)
        overflowError int = 
            "overflowing integer literal '" ++ show int ++ "'"

-- Parser for string literals. Rejects format strings
string :: ParserIO T.Text
string = lexeme $ P.char '"' >> (T.pack <$> P.manyTill stringChar (P.char '"')) 

-- Parser for the '\{' character
escapedBracket :: ParserIO Char
escapedBracket = P.hidden $ P.try (P.string "\\{") $> '{'

-- Parser for characters that can appear inside normal (unformatted) strings.
-- Bare '{' is forbidden and has to be escaped with '\'.
stringChar :: ParserIO Char
stringChar = (escapedFormat <|> L.charLiteral) <?> "character"
  where
    escapedFormat :: ParserIO Char
    escapedFormat = (P.try (P.char '{') >> fail "unescaped format bracket!")
                <|> (P.string "\\{" $> '{')

-- Parser for character literals.
character :: ParserIO PrimExpr
character = CharLit <$> (P.char '\'' *> L.charLiteral <* P.char '\'')

-- Parser for boolean literals ('True' or 'False')
boolean :: ParserIO PrimExpr
boolean = BoolLit <$> (keyword "True" $> True <|> keyword "False" $> False) 

-- Parser for units. Parens may be separated by whitespace.
unit :: ParserIO PrimExpr
unit = symbol "(" >> UnitLit <$ symbol ")"


{-# LANGUAGE OverloadedStrings #-}
module Parser.Identifier where

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Data.Text                     as T
import           Parser.Common
import           Control.Applicative            ( (<|>) )
import           Text.Megaparsec                ( (<?>) )
import           Control.Monad                  ( void )

uppercaseIdentifier :: Parser T.Text
uppercaseIdentifier = lexeme $ do
    first <- P.upperChar
    rest  <- P.many identifierChar
    return $ T.pack (first : rest)

identifierChar :: Parser Char
identifierChar = P.alphaNumChar <|> P.char '\'' <|> P.char '_'

keyword :: T.Text -> Parser ()
keyword kw = lexeme kwParser <?> ("keyword '" ++ T.unpack kw ++ "'")
  where
    kwParser = void . P.try $ (P.string kw <* P.notFollowedBy identifierChar)
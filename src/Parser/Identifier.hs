{-# LANGUAGE OverloadedStrings #-}
module Parser.Identifier where

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Data.Text                     as T
import           Parser.Common

uppercaseIdentifier :: Parser T.Text
uppercaseIdentifier = lexeme $ do
    first <- P.upperChar
    rest  <- P.many P.alphaNumChar
    return $ T.pack (first : rest)

{-# LANGUAGE OverloadedStrings #-}
module Parser.Identifier where

import qualified Text.Megaparsec               as P
import           Parser                         ( runParser )
import           Data.Text                      ( Text )
import           Parser.Common

uppercaseIdentifier :: Parser Text
uppercaseIdentifier = undefined

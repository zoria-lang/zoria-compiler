module Environment where

import Syntax
import ValueSyntax

import qualified Data.Map as M

data Environment = Environment (M.Map Identifier Value)

lookupVariableValue :: Identifier -> Environment -> Value
lookupVariableValue id (Environment env) = env M.! id


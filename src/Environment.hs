module Environment where

import Syntax
import ValueSyntax

import qualified Data.Map as M

data Environment = Environment (M.Map Identifier Value)

lookupVariableValue :: Identifier -> Environment -> Value
lookupVariableValue identifier (Environment env) = env M.! identifier

defineVariableValue :: Identifier -> Value -> Environment -> Environment
defineVariableValue identifier val (Environment env) = Environment (M.insert identifier val env)
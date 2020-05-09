module Environment where

import Syntax
import ValueSyntax

import qualified Data.Map as M
import qualified Data.Text as T

newtype Environment = Environment (M.Map T.Text Value)

emptyEnv :: Environment
emptyEnv = Environment M.empty

searchEnvironment :: T.Text -> Environment -> Value
searchEnvironment name (Environment env) = env M.! name

lookupIdentifier :: Identifier -> Environment -> Value
lookupIdentifier (Identifier name) = searchEnvironment name

lookupTypeVar :: TypeVar -> Environment -> Value
lookupTypeVar (TypeVar name) = searchEnvironment name

lookupConstructorName :: ConstructorName -> Environment -> Value
lookupConstructorName (ConstructorName name) = searchEnvironment name

lookupTypeName :: TypeName -> Environment -> Value
lookupTypeName (TypeName name) = searchEnvironment name

lookupModName :: ModName -> Environment -> Value
lookupModName (ModName name) = searchEnvironment name

extendEnvironment :: T.Text -> Value -> Environment -> Environment
extendEnvironment name val (Environment env) = Environment (M.insert name val env)

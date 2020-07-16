module Environment where

import           Syntax
import           ValueSyntax

import qualified Data.Map                      as M
import qualified Data.Text                     as T


emptyEnvironment :: Environment
emptyEnvironment = Environment M.empty

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

-- lookupModName :: ModName -> Environment -> Value
-- lookupModName (ModName name) = searchEnvironment name

lookupQualifiedVar :: ModName -> Identifier -> Environment -> Value
lookupQualifiedVar modName identifier env = undefined

lookupQualifiedConstructor :: ModName -> ConstructorName -> Environment -> Value
lookupQualifiedConstructor modName constructorName env = undefined 

lookupInternal :: Identifier -> Environment -> Value
lookupInternal identifier env = undefined

extendEnvironment :: T.Text -> Value -> Environment -> Environment
extendEnvironment name val (Environment env) =
    Environment (M.insert name val env)

insertIdentifier :: Identifier -> Value -> Environment -> Environment
insertIdentifier (Identifier name) val env = extendEnvironment name val env

unionEnvironments :: Environment -> Environment -> Environment
unionEnvironments (Environment leftEnv) (Environment rightEnv) =
    Environment $ M.union leftEnv rightEnv



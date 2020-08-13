module TypeChecker.Algorithm where

import Syntax
import Utility
import PrettyPrint(prettyPrint)

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set


import Control.Monad.Except
import Control.Monad.State


-- Type Scheme e.g.   ∀a,b.    a -> b -> a
data Scheme = Scheme [TypeVar] Type deriving(Show)


newtype Environment = Environment (Map.Map TypeVar Scheme) deriving(Show)
emptyEnv :: Environment
emptyEnv = Environment Map.empty

removeFromEnv :: TypeVar -> Environment -> Environment
removeFromEnv var (Environment env) = Environment $ Map.delete var env

extendEnv :: Environment -> (TypeVar, Scheme) -> Environment
extendEnv (Environment env) (var, scheme) = Environment $ Map.insert var scheme env

mergeEnvs :: Environment -> Environment -> Environment
mergeEnvs (Environment env1) (Environment env2) = Environment $ Map.union env1 env2 

-- Mapping from type variables to types
-- e.g. "a" has type Primitive BoolT
type Substitution = Map.Map TypeVar Type

emptySubstitution :: Substitution
emptySubstitution = Map.empty

substCompose :: Substitution -> Substitution -> Substitution
s1 `substCompose` s2 = (Map.map (apply s1) s2) `Map.union` s1


class Substitutable a where
  freeTypeVar :: a -> Set.Set TypeVar
  -- apply substitution: substitute all occurrences of type variables
  -- in 'a' which have a mapping in the Substitution
  apply       :: Substitution -> a -> a

instance Substitutable Type where
  freeTypeVar (TypeVariable t)      = Set.singleton t
  freeTypeVar (FunctionType t1 t2)  = (freeTypeVar t1) `Set.union` (freeTypeVar t2)
  freeTypeVar (PrimitiveType _)     = Set.empty
  freeTypeVar (TupleType types)     = freeTypeVar types
  freeTypeVar (NonPrimType _ )      = Set.empty
  freeTypeVar (ArrayType t)         = freeTypeVar t
  freeTypeVar (ParamType _ types)   = freeTypeVar types
  apply subst (TypeVariable x)      = case Map.lookup x subst of
                                        Nothing -> TypeVariable x
                                        Just t -> t
  apply subst (FunctionType t1 t2)  = FunctionType (apply subst t1) (apply subst t2)
  apply subst (TupleType types)     = TupleType (apply subst types)
  apply subst (ArrayType t)         = ArrayType (apply subst t)
  apply subst (ParamType name types)= ParamType name (apply subst types)
  -- PrimitiveType, NonPrimType
  apply subst t                     = t

instance Substitutable Scheme where
  freeTypeVar (Scheme vars t) = (freeTypeVar t) `Set.difference` (Set.fromList vars) 
  apply subst (Scheme vars t) = Scheme vars' t'
    where
      t' = (apply (foldr Map.delete subst vars) t)
      vars' = Set.toList $ freeTypeVar t' 

instance Substitutable a => Substitutable [a] where
  freeTypeVar l = foldr Set.union Set.empty (map freeTypeVar l) 
  apply substs = map (apply substs) 

instance Substitutable Environment where
  freeTypeVar (Environment env) = freeTypeVar (Map.elems env) 
  apply subst (Environment env) = Environment $ Map.map (apply subst) env 

-- convert scheme to type
-- e.g. from "∀a,b. a -> b -> a" to "a -> b -> a"
instantiate :: Scheme -> Inference Type
instantiate (Scheme vars t) = do
  newvars <- mapM (\_ -> newTypeVar) vars
  let s = Map.fromList(zip vars newvars) 
  return $ apply s t

-- yeild most general substitution that, when applied to both types makes them equal
-- e.g. if we unify (TypeVar a) and IntT we get a substitution [a -> IntT]
-- (BoolT -> IntT) with (a -> IntT) we get a substitution [a -> BoolT] 
unify :: Type -> Type -> Inference Substitution
unify (PrimitiveType v) (PrimitiveType w) | v == w = return emptySubstitution
unify (NonPrimType v)   (NonPrimType w)   | v == w = return emptySubstitution
unify (TypeVariable v) t = bindVariable v t
unify t (TypeVariable v) = bindVariable v t
unify (FunctionType l1 r1) (FunctionType l2 r2) = do
  subst1 <- unify l1 l2
  subst2 <- unify (apply subst1 r1) (apply subst1 r2)
  return (subst2 `substCompose` subst1)
unify (TupleType []) (TupleType []) =
  return (emptySubstitution)
unify (TupleType (t1:types1)) (TupleType (t2:types2)) = do
  subst1 <- unify t1 t2
  subst2 <- unify (TupleType types1) (TupleType types2) -- apply or not?
  return (subst2 `substCompose` subst1) 
unify (ArrayType t1) (ArrayType t2) = 
  unify t1 t2
unify (ParamType name1 []) (ParamType name2 []) | name1 == name2 = 
  return emptySubstitution
unify (ParamType name1 (t1:types1)) (ParamType name2 (t2:types2)) =
  if name1 == name2
  then do
    subst1 <- unify t1 t2
    subst2 <- unify (ParamType name1 types1) (ParamType name2 types2)
    return (subst2 `substCompose` subst1)
  else throwError $ "types do not unify: " 
                    ++ T.unpack (prettyPrint 0 t1) 
                    ++ " vs. " 
                    ++ T.unpack (prettyPrint 0 t2)

-- TODO: rest of the Types (PolymorphicParamType)

-- Error message should (maybe) eventually read as in haskell:
-- "Expected type: t vs. actual type: t2 at position..."
-- + TODO: error propagation (to comunicate the position). 
--         or pass on Position to lower functions?
unify t1 t2 = throwError $ "types do not unify: "
                           ++ T.unpack (prettyPrint 0 t1)
                           ++ " vs. "
                           ++ T.unpack (prettyPrint 0 t2)


bindVariable :: TypeVar -> Type -> Inference Substitution
bindVariable var typ
  | typ == TypeVariable var          = return emptySubstitution
  --                                   e.g. (\x -> x x)
  | var `Set.member` freeTypeVar typ = throwError $ "Var: \"" 
                                                    ++ T.unpack (prettyPrint 0 (TypeVariable var)) 
                                                    ++ "\" occurs in type: " 
                                                    ++ T.unpack (prettyPrint 0 typ)
  | otherwise                        = return (Map.singleton var typ)


type InferenceState = Int

type Inference a = ExceptT String (State InferenceState) a

-- Fresh type variable name generation
newTypeVar :: Inference Type
newTypeVar = do
  s <- get
  put (s + 1)
  return $ TypeVariable (intToTypeVar s) where
    letters :: [Char]
    letters = ['a'..'z']
    intToTypeVar :: Int -> TypeVar
    intToTypeVar s | s < 26 = TypeVar $ T.pack (letters!!s:[])
                   | otherwise = let (d,m) = (s `div` 26, s `mod` 26) in
                                    TypeVar $ T.pack ((letters!!m:[]) ++ show d)


runInference :: Inference a -> (Either String a, InferenceState)
runInference t = runState (runExceptT t) initState
  where initState = 0

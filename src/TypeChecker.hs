module TypeChecker where
  -- TODO: think about (maybe) changing the naming in Syntax.hs:
  -- TypeVariable, TypeVar, Identifier
  
  -- TODO: split TypeChecker.hs into smaller files

import Syntax
import Utility
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.State


-- Type Scheme e.g.   ∀a,b.    a -> b -> a
data Scheme = Scheme [TypeVar] Type

-- 
newtype Environment = Environment (Map.Map TypeVar Scheme)
emptyEnv :: Environment
emptyEnv = Environment Map.empty

removeFromEnv :: TypeVar -> Environment -> Environment
removeFromEnv var (Environment env) = Environment $ Map.delete var env

extendEnv :: Environment -> (TypeVar, Scheme) -> Environment
extendEnv (Environment env) (var, scheme) = Environment $ Map.insert var scheme env


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
  freeTypeVar (TupleType types)     = freeTypeVar (types)
  freeTypeVar (NonPrimType _ )      = Set.empty
  apply subst (TypeVariable x)      = case Map.lookup x subst of
                                        Nothing -> TypeVariable x
                                        Just t -> t
  apply subst (FunctionType t1 t2)  = FunctionType (apply subst t1) (apply subst t2)
  apply subst (TupleType types)     = TupleType (apply subst types)
  -- TODO: rest of  the types
  -- PrimitiveType, NonPrimType
  apply subst t                     = t

instance Substitutable Scheme where
  freeTypeVar (Scheme vars t) = (freeTypeVar t) `Set.difference` (Set.fromList vars) 
  apply subst (Scheme vars t) = Scheme vars (apply (foldr Map.delete subst vars) t) 

instance Substitutable a => Substitutable [a] where
  freeTypeVar l = foldr Set.union Set.empty (map freeTypeVar l) 
  apply substs = map (apply substs) 

instance Substitutable Environment where
  freeTypeVar (Environment env) = freeTypeVar (Map.elems env) 
  apply subst (Environment env) = Environment $ Map.map (apply subst) env 

type InferenceState = Int

type Inference a = ExceptT String (State InferenceState) a

runInference :: Inference a -> (Either String a, InferenceState)
runInference t = runState (runExceptT t) initState
  where initState = 0

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


-- convert scheme to type
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
  return (subst1 `substCompose` subst2)
unify (TupleType (t1:types1)) (TupleType (t2:types2)) = do
  subst1 <- unify t1 t2
  subst2 <- unify (TupleType types1) (TupleType types2) -- apply or not?
  return (subst1 `substCompose` subst2) 
-- TODO: rest of the Types

-- Error message should (maybe) eventually read as in haskell:
-- "Expected type: t vs. actual type: t2 at position..."
-- + TODO: error propagation (to comunicate the position). 
--         or pass on Position to lower functions?
unify t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2


bindVariable :: TypeVar -> Type -> Inference Substitution
bindVariable var typ
  | typ == TypeVariable var          = return emptySubstitution
  --                                   e.g. (\x -> x x)
  | var `Set.member` freeTypeVar typ = throwError $ "Var: " ++ show var ++ " occurs in type: " ++ show typ
  | otherwise                        = return (Map.singleton var typ)



-- Determine the type of an expression
infer :: Expr a -> Environment -> Inference (Substitution, Type)
infer (Primitive expr pos _ )                  _   = inferPrimitive expr
infer (Var id pos _ )                          env = inferVar id env 
infer (Lambda pattern expr id pos _ )          env = inferLambda pattern expr env
infer (LetIn (LetDef letdef) expr _ )          env = inferLet letdef expr env
infer (If condition trueExpr falseExpr pos _ ) env = inferIf condition trueExpr falseExpr env
infer (App expr1 expr2 _ )                     env = inferApp expr1 expr2 env
infer (Block exprs pos _ )                     env = inferBlock exprs env
infer (And lexpr rexpr pos _ )                 env = inferBools lexpr rexpr env
infer (Or lexpr rexpr pos _ )                  env = inferBools lexpr rexpr env
infer (AnnotatedExpr expr sig pos _ )          env = inferAnnotated expr sig env
infer (Tuple exprs pos _ )                     env = inferTuple exprs env
-- TODO: rest of the Expressions

inferBlock :: [Expr a] -> Environment -> Inference (Substitution, Type)
inferBlock [expr] env = do
  (sub,typ) <- infer expr env
  return (sub, typ)
inferBlock (e:exprs) env = do
  (sub1,type1) <- infer e env
   -- each expression (except for the last one) needs to return a () value 
  uniSub <- unify type1 (PrimitiveType UnitT)
  let subNext = uniSub `substCompose` sub1
  (sub2,type2) <- inferBlock exprs (apply subNext env)
  return (subNext `substCompose` sub2, type2)

inferTuple :: [Expr a] -> Environment -> Inference (Substitution, Type)
inferTuple [expr] env = do
      (sub,typ) <- infer expr env
      return(sub, TupleType [typ])
inferTuple (e:exprs) env = do
  (sub1,type1) <- infer e env
  (sub2,type2) <- inferTuple exprs env
  return (sub1 `substCompose` sub2, TupleType (type1:(fromTuple type2))) where
    fromTuple :: Type -> [Type]
    fromTuple (TupleType types) = types


inferAnnotated :: Expr a -> TypeSig -> Environment -> Inference (Substitution, Type)
inferAnnotated expr sig env = do
  (sub, typ) <- infer expr env
  unified <- unify typ (typeSig sig)
  return(unified `substCompose` sub, typ)

inferBools :: Expr a -> Expr a -> Environment -> Inference (Substitution, Type)
inferBools lexpr rexpr env = do
  (sub1,type1) <- infer lexpr env
  (sub2,type2) <- infer rexpr env
  uniBoolSub <- unify type1 (PrimitiveType BoolT)
  uniSidesSub <- unify type2 (apply uniBoolSub type1)
  return(uniSidesSub `substCompose` 
         uniBoolSub `substCompose`
         sub2 `substCompose` sub1, (PrimitiveType BoolT))

inferApp :: Expr a -> Expr a -> Environment -> Inference (Substitution, Type)
inferApp expr1 expr2 env = do
  typeVar <- newTypeVar
  (sub1, type1) <- infer expr1 env
  (sub2, type2) <- infer expr2 (apply sub1 env)
  sub3 <- unify (apply sub2 type1) (FunctionType type2 typeVar)
  return(sub3 `substCompose` sub2 `substCompose` sub1, apply sub3 typeVar)

inferIf :: Expr a -> Expr a -> Expr a -> Environment -> Inference (Substitution, Type)
inferIf condition trueExpr falseExpr env = do
  (csub,ctype) <- infer condition env
  (tsub,ttype) <- infer trueExpr env
  (fsub,ftype) <- infer falseExpr env
  unicsub <- unify ctype (PrimitiveType BoolT)
  unitfsub <- unify ttype ftype
  return (unitfsub `substCompose` 
          unicsub `substCompose` 
          fsub `substCompose` 
          tsub `substCompose` csub, apply unitfsub ttype)

inferLet :: Definition a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLet def expr env = case letPattern def of
  (VarPattern (Identifier id) pos _) -> inferLetVarPattern (TypeVar id) (letExpr def) expr env
  -- TODO: rest of the patterns
  _ -> undefined

inferLetVarPattern :: TypeVar -> Expr a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLetVarPattern defName defExpr expr env = do 
    (sub1, type1) <- infer defExpr env
    let env'          = apply sub1 env
     -- no generalization for now, link for reference
     -- https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf
    (sub2, type2) <- infer expr (extendEnv env' (defName, (Scheme [] type1))) 
    return (sub1 `substCompose` sub2, type2)

-- TODO: take in the name (Maybe T.Text)
inferLambda :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambda (ConstPattern primExpr pox _) expr env = do
  (pSub,pTyp) <- inferPrimitive primExpr
  (sub,typ) <- infer expr env
  return (sub, FunctionType pTyp typ)
inferLambda (VarPattern (Identifier id) pos _) expr env = do
  typeVar <- newTypeVar
  let env' = extendEnv env ((TypeVar id), (Scheme [] typeVar))
  (sub,typ) <- infer expr env'
  return  (sub, FunctionType (apply sub typeVar) typ)
-- TODO: rest of the patterns
inferLambda _ _ _ = undefined

inferPrimitive :: PrimExpr -> Inference (Substitution, Type)
inferPrimitive expr = case expr of
  IntLit _ -> return(emptySubstitution, PrimitiveType IntT)
  CharLit _ -> return(emptySubstitution, PrimitiveType CharT)
  FloatLit _ -> return(emptySubstitution, PrimitiveType FloatT)
  StringLit _ -> return(emptySubstitution, PrimitiveType StringT)
  BoolLit  _ -> return(emptySubstitution, PrimitiveType BoolT)
  UnitLit -> return(emptySubstitution, PrimitiveType UnitT)

 -- TODO: add position and throw better error
inferVar :: Identifier -> Environment -> Inference (Substitution, Type)
inferVar (Identifier id) (Environment env) = 
  case Map.lookup (TypeVar id) env of
    Nothing -> throwError $ "Unbound variable: " ++ show id
    Just s -> do 
      t <- instantiate s
      return (emptySubstitution, t) 


typeInference :: Environment -> Expr a -> Inference Type
typeInference env e =
    do  (s, t) <- infer e env
        return (apply s t)


testPosition = Utility.Position 0 ""
testIdX = Identifier (T.pack "x")
testVarPattern = (VarPattern testIdX testPosition ())
-- PrimitiveType BoolT
testPrimitiveBool = (Primitive (BoolLit False) testPosition ())
-- PrimitiveType IntT
testPrimitiveInt = Primitive (IntLit 6) testPosition ()
testLambdaVarToUnit = Lambda testVarPattern
                             (Primitive (UnitLit) testPosition ())
                             Nothing
                             testPosition
                             ()
-- a -> IntT
testLambdaVar = Lambda testVarPattern
                       testPrimitiveInt
                       -- (Var testIdX testPosition ()) 
                       Nothing 
                       testPosition
                       ()
-- Should fail with occur check
testLambdaVarOccur = Lambda testVarPattern
                            (App (Var testIdX testPosition ())
                                 (Var testIdX testPosition ()) ())
                            -- (Var testIdX testPosition ()) 
                            Nothing 
                            testPosition
                            ()
-- IntT -> BoolT
testLambdaConst = Lambda (ConstPattern (IntLit 6) testPosition ()) 
                         testPrimitiveBool
                         -- (Var testIdX testPosition ()) 
                         Nothing 
                         testPosition
                         ()
-- Should fail with type unification (BoolT -> BoolT) u (IntT -> BoolT)
testIf = If (Primitive (BoolLit True) testPosition ())
            (Lambda (ConstPattern (BoolLit True) testPosition ()) 
                    testPrimitiveBool
                    Nothing 
                    testPosition
                    ())
            testLambdaConst
            testPosition
            ()
-- Should fail with type unification
testApp = App testLambdaConst --testLambdaVar
              (Primitive (BoolLit False) testPosition ())
              ()
-- PrimitiveType BoolT
testLetVarPattern = LetIn (LetDef (Definition testVarPattern Nothing testPrimitiveInt testPosition))
                          (App testLambdaConst (Var testIdX testPosition ()) ())
                          ()

-- PrimitiveType BoolT
testAnd = And testLetVarPattern (App testLambdaConst testPrimitiveInt ()) testPosition ()

-- Should fail with type unification on comparing left and right
testOr = Or testLetVarPattern testPrimitiveInt testPosition ()

-- IntT -> BoolT
testAnnotated = AnnotatedExpr (testLambdaConst) 
                              (TypeSig [] (FunctionType (PrimitiveType IntT) (PrimitiveType BoolT))) 
                              testPosition 
                              ()

-- TupleType [BoolT, IntT]
testTuple1 = Tuple [testPrimitiveBool, testPrimitiveInt] testPosition ()
-- TupleType [BoolT, IntT, TupleType [BoolT, IntT]]
testTuple2 = Tuple [testPrimitiveBool, testPrimitiveInt, testTuple1] testPosition ()

-- PrimitiveType IntT
testBlock = LetIn ( LetDef (Definition (VarPattern (Identifier (T.pack "ignore")) testPosition ())
                           Nothing 
                           testLambdaVarToUnit 
                           testPosition))
                  ( Block [App (Var (Identifier (T.pack "ignore")) testPosition ()) testPrimitiveBool (),
                           Block [App (Var (Identifier (T.pack "ignore")) testPosition ()) testPrimitiveBool (),
                                  (Primitive (UnitLit) testPosition ())] testPosition (),
                           testPrimitiveInt]
                          testPosition
                          ())
                  ()
                    

test :: Show a => Expr a -> IO ()
test expr =
    let (res, _) = runInference (typeInference emptyEnv expr)
    in case res of
         Left err  ->  putStrLn $ "error: " ++ err
         Right t   ->  putStrLn $ show t


main :: IO ()
main = mapM_ test [testPrimitiveInt, -- pass 
                   testLambdaVar, -- pass
                   testLambdaVarOccur, -- fail
                   testLambdaConst, -- pass
                   testIf, -- fail
                   testApp, -- fail
                   testLetVarPattern, -- pass
                   testAnd, -- pass
                   testOr, -- fail
                   testAnnotated, -- pass
                   testTuple1, -- pass
                   testTuple2, -- pass
                   testBlock
                  ]


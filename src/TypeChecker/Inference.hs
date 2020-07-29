module TypeChecker.Inference where

import TypeChecker.Algorithm
import Syntax
import Utility

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set


import Control.Monad.Except
import Control.Monad.State


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
infer (Array exprs pos _ )                     env = inferArray exprs env
-- TODO: rest below
infer (Match expr cases pos _ )                env = inferMatch expr cases env
infer (Constructor name pos _ )                env = inferConstructor name env
infer (Internal id pos _ )                     env = inferInternal id env 
-- qualified should have another environment? other imports can(?) land in normal env
infer (QualifiedVar modName id pos _ )         env = inferQualifiedVar modName id env
infer (QualifiedConstructor modName con pos _ )env = inferQualifiedConstructor modName con env
infer (FormatString formatExpr pos _ )         env = inferFormatString formatExpr env

-- infer expr and unify with pattern types
-- unify cases' pattern types
-- unify cases' expression types
-- TODO: exhaustive match cases
inferMatch :: Expr a -> [MatchCase a] -> Environment -> Inference (Substitution, Type)
inferMatch expr cases env = do
  (subExpr, typeExpr) <- infer expr env
  (subCases, typeCases) <- inferMatchCases cases typeExpr (apply subExpr env)
  return(subCases `substCompose` subExpr, typeCases) 

inferMatchCases :: [MatchCase a] -> Type -> Environment -> Inference (Substitution, Type)
inferMatchCases [matchCase] typeExpr env = do
  (subPattern, typePattern)  <- inferPattern (matchCasePattern matchCase) env
  uniPatternSub              <- unify typePattern typeExpr
  (subCaseExpr,typeCaseExpr) <- infer (matchCaseExpr matchCase) (apply uniPatternSub env)
  return(subCaseExpr `substCompose` 
         uniPatternSub `substCompose` 
         subPattern, FunctionType typeExpr typeCaseExpr)
inferMatchCases (c:cases) typeExpr env = do
  (subPattern, typePattern)  <- inferPattern (matchCasePattern c) env
  uniPatternSub              <- unify typePattern typeExpr
  (subCaseExpr,typeCaseExpr) <- infer (matchCaseExpr c) (apply uniPatternSub env)
  (subTail, typeTail)        <- inferMatchCases cases typeExpr (apply subCaseExpr env)
  uniExprSub                 <- unify typeCaseExpr (resFromFunction typeTail)
  return (uniExprSub `substCompose`
          subTail `substCompose`
          subCaseExpr `substCompose`
          uniPatternSub `substCompose`
          subPattern, typeTail)
  where
    resFromFunction :: Type -> Type
    resFromFunction (FunctionType typeFrom typeRes) = typeRes


-- TODO: 
inferPattern :: Pattern a -> Environment -> Inference (Substitution, Type)
inferPattern pattern env = undefined
-- inferPattern [pattern] env = case pattern of
--   (ConstPattern primExpr pos _ ) -> do 
--     (sub,typ) <- inferPrimitive primExpr
--     return (emptySubstitution, typ)
--   (VarPattern (Identifier id) pos _ ) -> undefined
--   _ -> undefined
    

inferFormatString :: [FormatExpr a] -> Environment -> Inference (Substitution, Type)
inferFormatString formatExpr env = undefined -- TODO:

inferQualifiedVar :: ModName -> Identifier -> Environment -> Inference (Substitution, Type)
inferQualifiedVar modName id env = undefined -- TODO:

inferQualifiedConstructor :: ModName -> ConstructorName -> Environment -> Inference (Substitution, Type)
inferQualifiedConstructor modName con env = undefined -- TODO:

inferInternal :: Identifier -> Environment -> Inference (Substitution, Type)
inferInternal (Identifier id) (Environment env) = undefined -- TODO:

inferConstructor :: ConstructorName -> Environment -> Inference (Substitution, Type)
inferConstructor (ConstructorName name) (Environment env) = undefined -- TODO:
  -- case Map.lookup (TypeVar name) env of
  --   Nothing -> throwError $ "Not a type constructor : " ++ show id
  --   Just s -> return (emptySubstitution, t) 

inferArray :: [Expr a] -> Environment -> Inference (Substitution, Type)
inferArray [expr] env = do
  (sub,typ) <- infer expr env
  return(sub, typ)
inferArray (e:exprs) env = do
  (sub1,type1) <- infer e env
  (sub2,type2) <- inferArray exprs env
  uniSub <- unify type1 type2
  return (uniSub `substCompose`
          sub2 `substCompose` sub1, ArrayType type2)

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
  (ConstPattern primExpr pos _ )      -> undefined -- error? let 1 = expr??
  -- TODO: take letTypeSig in consideration
  (VarPattern (Identifier id) pos _ ) -> inferLetVarPattern (TypeVar id) (letExpr def) expr env
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
inferLambda pattern expr env = case pattern of
  pattern@ConstPattern{}       -> inferLambdaConst       pattern expr env
  pattern@TuplePattern{}       -> inferLambdaTuple       pattern expr env
  pattern@ConstructorPattern{} -> inferLambdaConstructor pattern expr env
  pattern@WildcardPattern{}    -> inferLambdaWildcard    pattern expr env
  pattern@VarPattern{}         -> inferLambdaVar         pattern expr env
  pattern@NamedPattern{}       -> inferLambdaNamed       pattern expr env

inferLambdaConst :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambdaConst (ConstPattern primExpr pox _) expr env = do
  (pSub,pTyp) <- inferPrimitive primExpr
  (sub,typ) <- infer expr env
  return (sub, FunctionType pTyp typ)

inferLambdaVar :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambdaVar (VarPattern (Identifier id) pos _) expr env = do
  typeVar <- newTypeVar
  let env' = extendEnv env ((TypeVar id), (Scheme [] typeVar))
  (sub,typ) <- infer expr env'
  return  (sub, FunctionType (apply sub typeVar) typ)

inferLambdaWildcard :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambdaWildcard (WildcardPattern pos _ ) expr env = do
  typeVar <- newTypeVar
  (sub,typ) <- infer expr env
  return(sub, FunctionType typeVar typ)

-- TODO:
inferLambdaConstructor :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambdaConstructor (ConstructorPattern conName patterns pos _ ) expr env = undefined

-- TODO:
inferLambdaTuple :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambdaTuple (TuplePattern patterns pos _ ) expr env = undefined 

-- TODO:
inferLambdaNamed :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambdaNamed (NamedPattern id pattern pos _ ) expr env = undefined

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


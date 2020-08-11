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


inferPattern :: Pattern a -> Environment -> Inference (Environment, Type)
inferPattern pattern env = case pattern of
  TuplePattern patterns pos _                -> inferTuplePattern patterns env
  NamedPattern (Identifier id) pattern pos _ -> inferNamedPattern (TypeVar id) pattern env
  VarPattern (Identifier id) pos _           -> inferVarPattern (TypeVar id) env
  ConstPattern expr pos _                    -> inferConstPattern expr env
  WildcardPattern pos _                      -> inferWildcardPattern env
  -- TODO: ConstructorPattern
  _ -> undefined



inferWildcardPattern :: Environment -> Inference (Environment, Type)
inferWildcardPattern env = do
  typeVar <- newTypeVar
  return(env, typeVar)

inferConstPattern :: PrimExpr -> Environment -> Inference (Environment, Type)
inferConstPattern expr env = do
  (_, exprType) <- inferPrimitive expr
  return(env, exprType)

inferVarPattern :: TypeVar -> Environment -> Inference (Environment, Type)
inferVarPattern name env = do
  typeVar <- newTypeVar
  let env'     = extendEnv env (name, Scheme [] typeVar)
  return(env', typeVar) 

inferNamedPattern :: TypeVar -> Pattern a -> Environment -> Inference (Environment, Type)
inferNamedPattern name pattern env = do
  (envPattern, typePattern) <- inferPattern pattern env
  let env' = extendEnv env (name, Scheme [] typePattern)
  return(env', typePattern)

-- Give a new environment back to the inference that called inferPattern
-- to infer the type of MatchCase Expression
inferTuplePattern :: [Pattern a] -> Environment -> Inference (Environment, Type)
inferTuplePattern [pattern] env = do
  (env',typePattern) <- inferPattern pattern env
  return (env', TupleType [typePattern])
inferTuplePattern (p:patterns) env = do
  (env',typePattern) <- inferPattern p env
  (tailEnv,tailType) <- inferTuplePattern patterns env
  return (mergeEnvs env' tailEnv, 
          TupleType (typePattern:(fromTuple tailType)))
  where
    fromTuple :: Type -> [Type]
    fromTuple (TupleType types) = types

inferConstructorPattern :: ConstructorName -> Type -> [Pattern a] -> Environment -> Inference (Environment,Type)
inferConstructorPattern (ConstructorName name) constrType [] env =
  case constrType of
    (FunctionType _ _ ) -> throwError $ "Type constructor " ++ show name ++ " expects more arguments"
    typ -> return (env, typ)
inferConstructorPattern constr@(ConstructorName name) constrType (pattern:tail) env =
  case constrType of
    NonPrimType _ -> errorTooManyArgs
    ParamType _ _ -> errorTooManyArgs
    FunctionType from to -> do
      (envPattern, typePattern) <- inferPattern pattern env
      uniPattern <- unify typePattern from
      (envTail, typeTail) <- inferConstructorPattern constr to tail env
      return (mergeEnvs envTail envPattern, typeTail)
  where
    errorTooManyArgs = 
      throwError $ "Too much arguments applied to constructor: " ++ show name
-- infer expr and unify with pattern types
-- unify cases' pattern types
-- unify cases' expression types
-- TODO: exhaustive match cases
inferMatch :: Expr a -> [MatchCase a] -> Environment -> Inference (Substitution, Type)
inferMatch expr cases env = do
  (subExpr, typeExpr)   <- infer expr env
  (subCases, typeCases) <- inferMatchCases cases typeExpr (apply subExpr env)
  return(subCases `substCompose` subExpr, typeCases) 

inferMatchCases :: [MatchCase a] -> Type -> Environment -> Inference (Substitution, Type)
inferMatchCases [matchCase] typeExpr env = do
  (envPattern, typePattern)  <- inferPattern (matchCasePattern matchCase) env
  uniPatternSub              <- unify typePattern typeExpr
  (subCaseExpr,typeCaseExpr) <- infer (matchCaseExpr matchCase) (apply uniPatternSub env)
  return(subCaseExpr `substCompose` 
         uniPatternSub, FunctionType typeExpr typeCaseExpr)
inferMatchCases (c:cases) typeExpr env = do
  (envPattern, typePattern)  <- inferPattern (matchCasePattern c) env
  uniPatternSub              <- unify typePattern typeExpr
  (subCaseExpr,typeCaseExpr) <- infer (matchCaseExpr c) (apply uniPatternSub env)
  (subTail, typeTail)        <- inferMatchCases cases typeExpr (apply subCaseExpr env)
  uniExprSub                 <- unify typeCaseExpr (resFromFunction typeTail)
  return (uniExprSub `substCompose`
          subTail `substCompose`
          subCaseExpr `substCompose`
          uniPatternSub, typeTail)
  where
    resFromFunction :: Type -> Type
    resFromFunction (FunctionType typeFrom typeRes) = typeRes
    

inferFormatString :: [FormatExpr a] -> Environment -> Inference (Substitution, Type)
inferFormatString formatExpr env = undefined -- TODO:

inferQualifiedVar :: ModName -> Identifier -> Environment -> Inference (Substitution, Type)
inferQualifiedVar modName id env = undefined -- TODO:

inferQualifiedConstructor :: ModName -> ConstructorName -> Environment -> Inference (Substitution, Type)
inferQualifiedConstructor modName con env = undefined -- TODO:

inferInternal :: Identifier -> Environment -> Inference (Substitution, Type)
inferInternal (Identifier id) (Environment env) = undefined -- TODO:

inferArray :: [Expr a] -> Environment -> Inference (Substitution, Type)
inferArray [expr] env = do
  (sub,typ)    <- infer expr env
  return(sub, typ)
inferArray (e:exprs) env = do
  (sub1,type1) <- infer e env
  (sub2,type2) <- inferArray exprs env
  uniSub       <- unify type1 type2
  return (uniSub `substCompose`
          sub2 `substCompose` sub1, ArrayType type2)

inferBlock :: [Expr a] -> Environment -> Inference (Substitution, Type)
inferBlock [expr] env = do
  (sub,typ)    <- infer expr env
  return (sub, typ)
inferBlock (e:exprs) env = do
  (sub1,type1) <- infer e env
   -- each expression (except for the last one) needs to return a () value 
  uniSub       <- unify type1 (PrimitiveType UnitT)
  let subNext  = uniSub `substCompose` sub1
  (sub2,type2) <- inferBlock exprs (apply subNext env)
  return (subNext `substCompose` sub2, type2)

inferTuple :: [Expr a] -> Environment -> Inference (Substitution, Type)
inferTuple [expr] env = do
      (sub,typ) <- infer expr env
      return(sub, TupleType [typ])
inferTuple (e:exprs) env = do
  (sub1,type1) <- infer e env
  (sub2,type2) <- inferTuple exprs env
  return (sub1 `substCompose` sub2, TupleType (type1:(fromTuple type2))) 
  where
    fromTuple :: Type -> [Type]
    fromTuple (TupleType types) = types

inferAnnotated :: Expr a -> TypeSig -> Environment -> Inference (Substitution, Type)
inferAnnotated expr sig env = do
  (exprSub, exprType) <- infer expr env
  unified             <- unify exprType (typeSig sig)
  return(unified `substCompose` exprSub, exprType)

inferBools :: Expr a -> Expr a -> Environment -> Inference (Substitution, Type)
inferBools lexpr rexpr env = do
  (subLeft,typeLeft)   <- infer lexpr env
  (subRight,typeRight) <- infer rexpr env
  uniBoolSub           <- unify typeLeft (PrimitiveType BoolT)
  uniSidesSub          <- unify typeRight (apply uniBoolSub typeLeft)
  return(uniSidesSub `substCompose` 
         uniBoolSub `substCompose`
         subRight `substCompose` subLeft, (PrimitiveType BoolT))

inferApp :: Expr a -> Expr a -> Environment -> Inference (Substitution, Type)
inferApp expr1 expr2 env = do
  typeVar       <- newTypeVar
  (sub1, type1) <- infer expr1 env
  (sub2, type2) <- infer expr2 (apply sub1 env)
  sub3          <- unify (apply sub2 type1) (FunctionType type2 typeVar)
  return(sub3 `substCompose` 
         sub2 `substCompose` sub1, apply sub3 typeVar)

inferIf :: Expr a -> Expr a -> Expr a -> Environment -> Inference (Substitution, Type)
inferIf condition trueExpr falseExpr env = do
  (condSub,condType) <- infer condition env
  (trueSub,trueType) <- infer trueExpr env
  (falseSub,falseType) <- infer falseExpr env
  uniCondSub <- unify condType (PrimitiveType BoolT)
  uniTrueFalseSub <- unify trueType falseType
  return (uniTrueFalseSub `substCompose` 
          uniCondSub `substCompose` 
          falseSub `substCompose` 
          trueSub `substCompose` condSub, apply uniTrueFalseSub trueType)

-- TODO: Only LetDef, no LetRecDef yet
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
  (sub,typ)   <- infer expr env
  return (sub, FunctionType pTyp typ)

inferLambdaVar :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambdaVar (VarPattern (Identifier id) pos _) expr env = do
  typeVar   <- newTypeVar
  let env'  = extendEnv env ((TypeVar id), (Scheme [] typeVar))
  (sub,typ) <- infer expr env'
  return  (sub, FunctionType (apply sub typeVar) typ)

inferLambdaWildcard :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambdaWildcard (WildcardPattern pos _ ) expr env = do
  typeVar   <- newTypeVar
  (sub,typ) <- infer expr env
  return(sub, FunctionType typeVar typ)

-- TODO:
inferLambdaConstructor :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambdaConstructor (ConstructorPattern conName patterns pos _ ) expr env = undefined

inferLambdaTuple :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambdaTuple pattern expr env = do
  (envPattern, typePattern) <- inferPattern pattern env
  (subExpr, typeExpr)       <- infer expr envPattern
  return(subExpr, FunctionType (apply subExpr typePattern) typeExpr)

inferLambdaNamed :: Pattern a -> Expr a -> Environment -> Inference (Substitution, Type)
inferLambdaNamed (NamedPattern (Identifier id) pattern pos _ ) expr env = do
  (envPattern, typePattern) <- inferPattern pattern env
  (subExpr, typeExpr)       <- infer expr (extendEnv envPattern ((TypeVar id), Scheme [] typePattern))
  return(subExpr, FunctionType (apply subExpr typePattern) typeExpr)
  
inferPrimitive :: PrimExpr -> Inference (Substitution, Type)
inferPrimitive expr = case expr of
  IntLit    _ -> return(emptySubstitution, PrimitiveType IntT)
  CharLit   _ -> return(emptySubstitution, PrimitiveType CharT)
  FloatLit  _ -> return(emptySubstitution, PrimitiveType FloatT)
  StringLit _ -> return(emptySubstitution, PrimitiveType StringT)
  BoolLit   _ -> return(emptySubstitution, PrimitiveType BoolT)
  UnitLit     -> return(emptySubstitution, PrimitiveType UnitT)

inferVar :: Identifier -> Environment -> Inference (Substitution, Type)
inferVar (Identifier id) (Environment env) = 
  case Map.lookup (TypeVar id) env of
    Nothing -> throwError $ "Unbound variable: " ++ show id
    Just scheme  -> do 
      typeOfVar <- instantiate scheme
      return (emptySubstitution, typeOfVar) 

inferConstructor :: ConstructorName -> Environment -> Inference (Substitution, Type)
inferConstructor (ConstructorName name) (Environment env) = 
  case Map.lookup (TypeVar name) env of
    Nothing -> throwError $ "Undeclared type constructor: " ++ show name
    Just scheme -> do
      typeOfConstructor <- instantiate scheme
      return (emptySubstitution, typeOfConstructor)

inferExpr :: Environment -> Expr a -> Inference Type
inferExpr env e =
    do  (s, t) <- infer e env
        return (apply s t)


-- TODO: Observation: Either environment should be extended 
--                    (e.g. Map (T.Text, type) -> Scheme) - type as in TypeVar or TypeName etc.)
--                    - solves checking for type/constructor/var declarations
--                      but leaves exhaustive matching not feasable (i think)
--                  OR same as above, but Map (T.Text, type, numOfConstr) -> Scheme
--                     with numOfConstr only being used/useful for type == TypeName
--                     this may solve the problem with exhaustive matching 
--                  OR another data structure for types and their constructors is needed (what then?)
typecheckTypeDef :: TDef -> Environment -> Either String Environment
typecheckTypeDef (TDef typeName params kind cases _ ) env = 
  -- add typeName to env / check for previous declaration as above
  constructorsToEnv cases typeName params env


  -- TODO: check for previous constructor declarations
constructorsToEnv :: [TypeCase] -> TypeName -> [TypeVar] -> Environment -> Either String Environment
constructorsToEnv [] (TypeName typeName) _ _ = 
  Left $ "No type constructors for type: " ++ (T.unpack typeName)
constructorsToEnv [(TypeCase (ConstructorName name) types _ )] typeName params env =
  case params of
    [] -> 
      Right $ extendEnv env (TypeVar name, Scheme [] (foldr FunctionType 
                                                            (NonPrimType typeName) 
                                                            types))
    xs -> 
      Right $ extendEnv env (TypeVar name, Scheme params 
                                                  (foldr FunctionType 
                                                         (ParamType typeName (map TypeVariable params)) 
                                                         types))  
constructorsToEnv ((TypeCase (ConstructorName name) types _ ):tail) typeName params env =
  case params of
    [] -> 
      let env' = extendEnv env ((TypeVar name),
                                    Scheme [] (NonPrimType typeName))
      in constructorsToEnv tail typeName params env'
    xs -> 
      let env' = extendEnv env ((TypeVar name), 
                                  Scheme params 
                                         (foldr FunctionType 
                                                (ParamType typeName (map TypeVariable params)) 
                                                types))  
      in constructorsToEnv tail typeName params env'

typecheckAlias :: TAlias -> Environment -> Either String Environment
typecheckAlias (TAlias (TypeName name) params kind alType _ ) env =
  -- TODO: check if params are in type / check type correctness?
  --        or is it done in parsing? 
  -- TODO: rework
  Right $ extendEnv env ((TypeVar name), Scheme params alType)


typecheckTopLevelLet :: Definition a -> Environment -> Either String Environment
typecheckTopLevelLet def env = case letPattern def of
  -- TODO: take letTypeSig in consideration
  (VarPattern (Identifier id) pos _ ) -> 
    let res = inferLetDefVarPattern (letExpr def) env
    in case res of
        Left err -> Left err
        Right t  -> Right (extendEnv env (TypeVar id, Scheme [] t))
  -- TODO: rest of the patterns
  _ -> undefined

inferLetDefVarPattern :: Expr a -> Environment -> Either String Type
inferLetDefVarPattern defExpr env =
    -- maybe take the defName into the env (either here or higher up)
    let (res, _ ) = runInference $ inferExpr env defExpr
    in res
module Evaluator(eval) where

import           ValueSyntax
import           Syntax
import           Environment

evalError :: Show a => Expr a -> String -> b
evalError expr typeString =
    error
        $  "Expression"
        ++ show expr
        ++ "\ncannot be evaluated to type: "
        ++ typeString

exprTypeError :: Show a => Expr a -> String -> b
exprTypeError expr typeString =
    error $ "Expression" ++ show expr ++ "\nis not of type " ++ typeString

evalPrimExpr :: PrimExpr -> PrimVal
evalPrimExpr (IntLit   n) = IntVal n
evalPrimExpr (CharLit  c) = CharVal c
evalPrimExpr (FloatLit f) = FloatVal f
evalPrimExpr (BoolLit  b) = BoolVal b
evalPrimExpr UnitLit      = UnitVal

evalPrimitive :: Show a => Expr a -> IO Value
evalPrimitive (Primitive primExpr _ _) =
    return $ PrimitiveVal $ evalPrimExpr primExpr

evalVar :: Show a => Expr a -> Environment -> IO Value
evalVar (Var identifier _ _) env = return $ lookupIdentifier identifier env

evalConstructor :: Show a => Expr a -> Environment -> IO Value
evalConstructor (Constructor constructorName _ _) env =
    return $ lookupConstructorName constructorName env

evalQualifiedVar :: Show a => Expr a -> Environment -> IO Value
evalQualifiedVar (QualifiedVar modName identifier _ _) env = 
    return $ lookupQualifiedVar modName identifier env

evalQualifiedConstructor :: Show a => Expr a -> Environment -> IO Value
evalQualifiedConstructor (QualifiedConstructor modName constructorName _ _) env= 
    return $ lookupQualifiedConstructor modName constructorName env

evalAnd :: Show a => Expr a -> Environment -> IO Value
evalAnd (And leftExpr rightExpr _ _) env = do
    PrimitiveVal (BoolVal leftVal) <- eval leftExpr env
    if leftVal
        then eval rightExpr env
        else return $ PrimitiveVal (BoolVal False)

evalOr :: Show a => Expr a -> Environment -> IO Value
evalOr (Or leftExpr rightExpr _ _) env = do
  PrimitiveVal (BoolVal leftVal) <- eval leftExpr env
  if leftVal
    then return (PrimitiveVal (BoolVal True))    
    else eval rightExpr env 

evalIf :: Show a => Expr a -> Environment -> IO Value
evalIf (If predicate consequent alternative _ _) env = do
    predicateVal <- eval predicate env
    case predicateVal of
        PrimitiveVal (BoolVal True ) -> eval consequent env
        PrimitiveVal (BoolVal False) -> eval alternative env
        _                            -> evalError predicate "Bool"

evalBlock :: Show a => Expr a -> Environment -> IO Value
evalBlock (Block exprs _ _) env = evalSequence exprs  where
    evalSequence :: Show a => [Expr a] -> IO Value
    evalSequence []                      = return $ PrimitiveVal $ UnitVal
    evalSequence [expr                 ] = eval expr env
    evalSequence (firstExpr : restExprs) = do
        eval firstExpr env
        evalSequence restExprs

exprUnit :: Expr a -> Expr ()
exprUnit = fmap (const ())

evalLambda :: Show a => Expr a -> Environment -> IO Value
evalLambda lambdaExpr@Lambda{} env = return
    $ makeProcedure (exprUnit lambdaExpr) env  where
        makeProcedure :: Expr () -> Environment -> Value
        makeProcedure (Lambda pattern expr name _ _) env =
            Procedure pattern expr env

evalExprList :: Show a => [Expr a] -> Environment -> IO [Value]
evalExprList exprList env = mapM (\expr -> eval expr env) exprList

evalTuple :: Show a => Expr a -> Environment -> IO Value
evalTuple (Tuple exprList _ _) env = do
    valList <- evalExprList exprList env
    return $ TupleVal valList

evalArray :: Show a => Expr a -> Environment -> IO Value
evalArray (Array exprList _ _) env = do
    valList <- evalExprList exprList env
    return $ TupleVal valList
    
matchPatternList :: [Pattern a] -> [Value] -> Maybe Environment
matchPatternList [] [] = Just emptyEnvironment
matchPatternList [] _ = Nothing
matchPatternList _ [] = Nothing
matchPatternList (pattern : restPatterns) (val : restVals) = do
    firstMatch <- matchPattern pattern val
    restMatch  <- matchPatternList restPatterns restVals
    return (unionEnvironments firstMatch restMatch)

matchPattern :: Pattern a -> Value -> Maybe Environment
matchPattern (WildcardPattern _ _) _ = Just emptyEnvironment
matchPattern (ConstPattern primExpr _ _) (PrimitiveVal primVal)
    | (evalPrimExpr primExpr) == primVal = Just emptyEnvironment
    | otherwise                          = Nothing
matchPattern (TuplePattern patternList _ _) (TupleVal valList)
    | (length patternList) == (length valList) =
        matchPatternList patternList valList
    | otherwise = Nothing
matchPattern TuplePattern{} _ = Nothing
matchPattern (VarPattern identifier _ _) val =
        Just $ insertIdentifier identifier val emptyEnvironment
matchPattern (NamedPattern identifier pattern _ _) val = do
    specificMatch <- matchPattern pattern val
    return $ insertIdentifier identifier val specificMatch
matchPattern (ConstructorPattern patternConstructorName patterns _ _)
            (CustomVal valConstructorName values)
    | patternConstructorName == valConstructorName = matchPatternList patterns values
    | otherwise = Nothing
matchPattern (ConstructorPattern {}) _ = Nothing

apply :: Value -> Value -> IO Value
apply (Procedure pattern body procEnv) argument = 
    case matchPattern pattern argument of
        Just applyEnv -> eval body (unionEnvironments procEnv applyEnv)
apply (InternalFun f) (CustomVal listConstructorName xs) = f xs
-- TODO: Replace listConstructorName with actual name of list constructor.
apply someOperator someArgument = undefined
-- TODO: apply to something else?

evalApp :: Show a => Expr a -> Environment -> IO Value
evalApp (App operator argument _) env = do
    opVal  <- eval operator env
    argVal <- eval argument env
    apply opVal argVal

evalMatch :: Show a => Expr a -> Environment -> IO Value
evalMatch (Match expr matchCases _ _) env =  do
    value <- eval expr env
    firstMatch value matchCases env where
        firstMatch _ [] env = error "No match"
        firstMatch val ((MatchCase pattern body):nextCases) env =
            case matchPattern pattern val of
                (Just applyEnv) -> eval body (unionEnvironments applyEnv env)
                Nothing -> firstMatch val nextCases env

evalExternal :: Show a => Expr a -> Environment -> IO Value
evalExternal (External filePath name _ _) env = undefined
-- TODO: Parse external file and find identifier value?


evalInternal :: Show a => Expr a -> Environment -> IO Value
evalInternal (Internal identifier _ _) env = 
    return $ lookupInternal identifier env

evalAnnotatedExpr :: Show a => Expr a -> Environment -> IO Value
evalAnnotatedExpr (AnnotatedExpr expr _ _ _) env = eval expr env

evalFormatString :: Show a => Expr a -> Environment -> IO Value
evalFormatString (FormatString formatExpr _ _) env = undefined
-- TODO: What should string look like? Decide and write eval

eval :: Show a => Expr a -> Environment -> IO Value
eval expr@Primitive{}            env = evalPrimitive expr
eval expr@Var{}                  env = evalVar expr env
eval expr@Constructor{}          env = evalConstructor expr env
eval expr@QualifiedVar{}         env = evalQualifiedVar expr env
eval expr@QualifiedConstructor{} env = evalQualifiedConstructor expr env
eval expr@And{}                  env = evalAnd expr env
eval expr@Or{}                   env = evalOr expr env
eval expr@If{}                   env = evalIf expr env
eval expr@Block{}                env = evalBlock expr env
eval expr@Lambda{}               env = evalLambda expr env
eval expr@Tuple{}                env = evalTuple expr env
eval expr@App{}                  env = evalApp expr env
eval expr@Match{}                env = evalMatch expr env
eval expr@External{}             env = evalExternal expr env
eval expr@Internal{}             env = evalInternal expr env
eval expr@AnnotatedExpr{}        env = evalAnnotatedExpr expr env
eval expr@FormatString{}         env = evalFormatString expr env
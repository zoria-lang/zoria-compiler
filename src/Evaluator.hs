module Evaluator where

import           ValueSyntax
import           Syntax
import           Environment
import           Utility

import qualified Data.Text                     as T

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

evalPrimitive :: Expr a -> Environment -> IO Value
evalPrimitive (Primitive primExpr _ _) env =
    return $ PrimitiveVal $ evalPrimExpr primExpr


evalVar :: Expr a -> Environment -> IO Value
evalVar (Var identifier _ _) env = return $ lookupIdentifier identifier env

evalConstructor :: Expr a -> Environment -> IO Value
evalConstructor (Constructor constructorName _ _) env =
    return $ lookupConstructorName constructorName env

evalAnd :: Show a => Expr a -> Environment -> IO Value
evalAnd (And leftExpr rightExpr _ _) env = do
    leftVal <- eval leftExpr env
    case leftVal of
        PrimitiveVal (BoolVal False) -> return leftVal
        PrimitiveVal (BoolVal True ) -> do
            rightVal <- eval rightExpr env
            case rightVal of
                PrimitiveVal (BoolVal _) -> return rightVal
                _                        -> evalError rightExpr "bool"
        _ -> evalError leftExpr "Bool"
evalAnd expr _ = exprTypeError expr "And"

evalOr :: Show a => Expr a -> Environment -> IO Value
evalOr (Or leftExpr rightExpr _ _) env = do
    leftVal <- eval leftExpr env
    case leftVal of
        PrimitiveVal (BoolVal True ) -> return leftVal
        PrimitiveVal (BoolVal False) -> do
            rightVal <- eval rightExpr env
            case rightVal of
                PrimitiveVal (BoolVal _) -> return rightVal
                _                        -> error $ show rightExpr
        _ -> evalError leftExpr "Bool"
evalOr expr _ = exprTypeError expr "Or"

evalIf :: Show a => Expr a -> Environment -> IO Value
evalIf (If predicate consequent alternative _ _) env = do
    predicateVal <- eval predicate env
    case predicateVal of
        PrimitiveVal (BoolVal True ) -> eval consequent env
        PrimitiveVal (BoolVal False) -> eval alternative env
        _                            -> evalError predicate "Bool"
evalIf expr _ = exprTypeError expr "If"

evalBlock :: Show a => Expr a -> Environment -> IO Value
evalBlock (Block exprs _ _) env = evalSequence exprs  where
    evalSequence :: Show a => [Expr a] -> IO Value
    evalSequence []                      = return $ PrimitiveVal $ UnitVal
    evalSequence [expr                 ] = eval expr env
    evalSequence (firstExpr : restExprs) = do
        eval firstExpr env
        evalSequence restExprs
evalBlock expr _ = exprTypeError expr "Block"

exprUnit :: Expr a -> Expr ()
exprUnit = fmap (const ())

evalLambda :: Show a => Expr a -> Environment -> IO Value
evalLambda lambdaExpr@Lambda{} env = return
    $ makeProcedure (exprUnit lambdaExpr) env  where
    makeProcedure :: Expr () -> Environment -> Value
    makeProcedure (Lambda pattern expr name _ _) env =
        Procedure pattern expr env
evalLambda expr _ = exprTypeError expr "Lambda"

evalExprList :: Show a => [Expr a] -> Environment -> IO [Value]
evalExprList []            env = return []
evalExprList (expr : tail) env = do
    firstVal <- eval expr env
    restVals <- evalExprList tail env
    return $ firstVal : restVals

evalTuple :: Show a => Expr a -> Environment -> IO Value
evalTuple (Tuple exprList _ _) env = do
    valList <- evalExprList exprList env
    return $ TupleVal valList
evalTuple expr _ = exprTypeError expr "Tuple"

evalArray :: Show a => Expr a -> Environment -> IO Value
evalArray (Array exprList _ _) env = do
    valList <- evalExprList exprList env
    return $ TupleVal valList
evalArray expr _ = exprTypeError expr "Array"

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
matchPattern ConstPattern{} _ = Nothing
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
            (CustomType valConstructorName values)
    | patternConstructorName == valConstructorName = matchPatternList patterns values
    | otherwise = Nothing
matchPattern (ConstructorPattern {}) _ = Nothing

-- This next!!!
apply :: Value -> Value -> IO Value
apply (Procedure pattern body procEnv) argument = 
    case matchPattern pattern argument of
        Just applyEnv -> eval body (unionEnvironments procEnv applyEnv)

evalApp :: Show a => Expr a -> Environment -> IO Value
evalApp (App operator argument _) env = do
    opVal  <- eval operator env
    argVal <- eval argument env
    apply opVal argVal
evalApp expr _ = exprTypeError expr "App"

evalMatch :: Show a => Expr a -> Environment -> IO Value
evalMatch (Match expr matchCases _ _) env =  do
    value <- eval expr env
    firstMatch value matchCases env where
        firstMatch _ [] env = error "No match"
        firstMatch val ((MatchCase pattern body):nextCases) env =
            case matchPattern pattern val of
                (Just applyEnv) -> eval body (unionEnvironments applyEnv env)
                Nothing -> firstMatch val nextCases env            

evalAnnotatedExpr :: Show a => Expr a -> Environment -> IO Value
evalAnnotatedExpr (AnnotatedExpr expr _ _ _) env = eval expr env

eval :: Show a => Expr a -> Environment -> IO Value
eval expr@Primitive{}            env = evalPrimitive expr env
eval expr@Var{}                  env = evalVar expr env
eval expr@Constructor{}          env = evalConstructor expr env
eval expr@QualifiedVar{}         env = undefined -- !!!
eval expr@QualifiedConstructor{} env = undefined -- !!!
eval expr@And{}                  env = evalAnd expr env
eval expr@Or{}                   env = evalOr expr env
eval expr@If{}                   env = evalIf expr env
eval expr@Block{}                env = evalBlock expr env
eval expr@Lambda{}               env = evalLambda expr env
eval expr@Tuple{}                env = evalTuple expr env
eval expr@App{}                  env = evalApp expr env
eval expr@Match{}                env = evalMatch expr env
eval expr@External{}             env = undefined -- !!!
eval expr@Internal{}             env = undefined -- !!!
eval expr@AnnotatedExpr{}        env = evalAnnotatedExpr expr env
eval expr@FormatString{}         env = undefined -- !!!

-- REMOVE LATER
testPosition :: Utility.Position
testPosition = Position 0 ""

e :: Expr Integer
e = Var (Identifier (T.pack "x")) testPosition 0

e2 = Block [] testPosition 0

env :: Environment
env = extendEnvironment (T.pack "x") (PrimitiveVal (IntVal 3)) emptyEnvironment

e3 = Lambda
    (WildcardPattern testPosition ())
    (And (Primitive (BoolLit True) testPosition ())
         (Primitive (BoolLit True) testPosition ())
         testPosition
         ()
    )
    Nothing
    testPosition
    ()

pattern :: Pattern ()
pattern = ConstPattern (IntLit 4) testPosition ()

-- val :: Value
-- val = PrimitiveVal (IntVal 5)

t :: Expr ()
t = Tuple [Tuple [Primitive (IntLit 1) testPosition (),
                Primitive (IntLit 2) testPosition ()
                ] testPosition (),
                Primitive (IntLit 3) testPosition ()
                ] testPosition ()--,
        -- Primitive (BoolLit False) testPosition ()
        -- Primitive (FloatLit 4.5) testPosition ()] testPosition ()


makeIdentifier :: String -> Identifier
makeIdentifier str = (Identifier (T.pack str))

identifiers = map makeIdentifier ["x","y"] --,"z"]

varPatterns = map (\id -> VarPattern id testPosition ()) identifiers

tp = TuplePattern varPatterns testPosition ()

tp2 = TuplePattern [tp, VarPattern (makeIdentifier "z") testPosition ()] testPosition ()

namedPatternExample = NamedPattern (makeIdentifier "tuple") tp2 testPosition ()

vals = eval t emptyEnvironment

result = do
    v <- vals 
    return $ matchPattern tp2 v


result2 = do
    v <- vals 
    return $ matchPattern namedPatternExample v


cp = ConstPattern (IntLit 3) testPosition ()

cpTest = matchPattern cp (PrimitiveVal (IntVal 3))
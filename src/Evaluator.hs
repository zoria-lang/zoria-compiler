module Evaluator where

import ValueSyntax
import Syntax
import Environment

evalError :: Show a => Expr a -> String -> b
evalError expr typeString =
    error $ "Expression" ++
    show expr ++
    "\ncannot be evaluated to type: " ++ typeString

-- exprTypeError :: Show a => Expr a -> String -> b


evalPrimitive :: PrimExpr -> PrimVal
evalPrimitive (IntLit   n) = IntVal n
evalPrimitive (CharLit  c) = CharVal c
evalPrimitive (FloatLit f) = FloatVal f
evalPrimitive (BoolLit  b) = BoolVal b
evalPrimitive UnitLit      = UnitVal

evalVar :: Expr a -> Environment -> Value
evalVar (Var identifier position a) env = lookupVariableValue identifier env

evalAnd :: Show a => Expr a -> Environment -> IO Value
evalAnd (And leftExpr rightExpr position a) env = do
    leftVal <- eval leftExpr env
    case leftVal of
        PrimitiveVal (BoolVal False) -> return leftVal 
        PrimitiveVal (BoolVal True) -> do
            rightVal <- eval rightExpr env
            case rightVal of
                PrimitiveVal (BoolVal _) -> return rightVal
                _ -> evalError rightExpr "bool"
        _ -> error $ show leftExpr
evalAnd expr _ = error $ show expr ++ "is not And"

evalOr :: Show a => Expr a -> Environment -> IO Value
evalOr (Or leftExpr rightExpr position a) env = do
    leftVal <- eval leftExpr env
    case leftVal of
        PrimitiveVal (BoolVal True) -> return leftVal 
        PrimitiveVal (BoolVal False) -> do
            rightVal <- eval rightExpr env
            case rightVal of
                PrimitiveVal (BoolVal _) -> return rightVal
                _ -> error $ show rightExpr
        _ -> error $ show leftExpr 
evalOr expr _ = error $ show expr ++ "is not Or"

evalIf :: Expr a -> Environment -> IO Value
evalIf (If predicate consequent alternative position a) env = do
    predicateVal <- eval predicate env
    case predicateVal of
        PrimitiveVal (BoolVal True) -> eval consequent env
        PrimitiveVal (BoolVal False) -> eval alternative env
        _ -> error "Cannot eval"

eval :: Expr a -> Environment -> IO Value
eval (Primitive expr position a) env = return $ PrimitiveVal $ evalPrimitive expr



module Evaluator where

import ValueSyntax
import Syntax
import Environment
import Utility

evalError :: Show a => Expr a -> String -> b
evalError expr typeString =
    error $ "Expression" ++
    show expr ++
    "\ncannot be evaluated to type: " ++ typeString

exprTypeError :: Show a => Expr a -> String -> b
exprTypeError expr typeString = 
    error $ "Expression" ++
    show expr ++
    "\nis not of type " ++ typeString

evalPrimitive :: Expr a -> Environment -> IO Value
evalPrimitive (Primitive primExpr position a) env =
    return $ PrimitiveVal $ evalPrimExpr primExpr where
        evalPrimExpr :: PrimExpr -> PrimVal
        evalPrimExpr (IntLit   n) = IntVal n
        evalPrimExpr (CharLit  c) = CharVal c
        evalPrimExpr (FloatLit f) = FloatVal f
        evalPrimExpr (BoolLit  b) = BoolVal b
        evalPrimExpr UnitLit      = UnitVal

evalVar :: Expr a -> Environment -> IO Value
evalVar (Var identifier position a) env = return $ lookupIdentifier identifier env

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
evalAnd expr _ = exprTypeError expr "And"

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

evalIf :: Show  a => Expr a -> Environment -> IO Value
evalIf (If predicate consequent alternative position a) env = do
    predicateVal <- eval predicate env
    case predicateVal of
        PrimitiveVal (BoolVal True) -> eval consequent env
        PrimitiveVal (BoolVal False) -> eval alternative env
        _ -> evalError predicate "Bool"
evalIf expr _ = error $ show expr ++ "is not If"

evalBlock :: Show a => Expr a -> Environment -> IO Value
evalBlock (Block exprs position a) env = evalSequence exprs where
    evalSequence :: Show a => [Expr a] -> IO Value
    evalSequence [] = return $ PrimitiveVal $ UnitVal
    evalSequence [expr] = eval expr env
    evalSequence (firstExpr:restExprs) = do
        eval firstExpr env
        evalSequence restExprs

eval :: Show a => Expr a -> Environment -> IO Value
eval expr@Primitive {} env = evalPrimitive expr env
eval expr@Var {} env = evalVar expr env
eval expr@And {} env = evalAnd expr env
eval expr@Or {} env = evalOr expr env
eval expr@If {} env = evalIf expr env

-- REMOVE LATER
testPosition :: Utility.Position
testPosition = Position 0 ""

e :: Expr Integer
e = And (Primitive (BoolLit False) testPosition 0) (Primitive (BoolLit True) testPosition 0)
        testPosition 0

        
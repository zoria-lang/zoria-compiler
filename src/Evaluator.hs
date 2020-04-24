module Evaluator where

import ValueSyntax
import Syntax
import Environment


evalPrimitive :: PrimExpr -> PrimVal
evalPrimitive (IntLit   n) = IntVal n
evalPrimitive (CharLit  c) = CharVal c
evalPrimitive (FloatLit f) = FloatVal f
evalPrimitive (BoolLit  b) = BoolVal b
evalPrimitive UnitLit      = UnitVal

evalVar :: Expr a -> Environment -> Value
evalVar (Var identifier position a) env = lookupVariableValue identifier env

eval :: Expr a -> Environment -> IO Value
eval (Primitive expr position a) env = return $ PrimitiveVal $ evalPrimitive expr



{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.EvaluatorTests.EvalTests
    ( htf_thisModulesTests
    )
where

import qualified Data.Text as T

import Test.Framework
import Syntax
import Utility
import Evaluator
import Evaluator.Environment
import Evaluator.ValueSyntax

p :: Position
p = Position 0 ""

-- Primitive expressions

primitiveTrueLit = Primitive (BoolLit True) p ()
primitiveFalseLit = Primitive (BoolLit False) p ()
primitiveIntLit = Primitive (IntLit 0) p ()
primitiveFloatLit = Primitive (FloatLit 1.5) p ()
primitiveStringLit = Primitive (StringLit (T.pack "test")) p ()
primitiveUnitLit = Primitive UnitLit p () 

test_primitive = do
    PrimitiveVal (IntVal x) <- eval primitiveIntLit emptyEnvironment
    assertEqual x 0
    PrimitiveVal (FloatVal x) <- eval primitiveFloatLit emptyEnvironment
    assertEqual x 1.5
    PrimitiveVal (BoolVal x) <- eval primitiveTrueLit emptyEnvironment
    assertEqual x True
    PrimitiveVal (BoolVal x) <- eval primitiveFalseLit emptyEnvironment
    assertEqual x False
    PrimitiveVal (StringVal text) <- eval primitiveStringLit emptyEnvironment
    assertEqual (T.unpack text) "test"
    PrimitiveVal x <- eval primitiveUnitLit emptyEnvironment
    assertEqual x UnitVal

-- And, Or

trueAndTrue = And primitiveTrueLit primitiveTrueLit p ()
trueAndFalse = And primitiveTrueLit primitiveFalseLit p ()
falseAndTrue = And primitiveFalseLit primitiveTrueLit p ()
falseAndFalse = And primitiveFalseLit primitiveFalseLit p ()

test_and = do
    PrimitiveVal (BoolVal x) <- eval trueAndTrue emptyEnvironment
    assertEqual x True
    PrimitiveVal (BoolVal x) <- eval trueAndFalse emptyEnvironment
    assertEqual x False
    PrimitiveVal (BoolVal x) <- eval falseAndTrue emptyEnvironment
    assertEqual x False
    PrimitiveVal (BoolVal x) <- eval falseAndFalse emptyEnvironment
    assertEqual x False


trueOrTrue = Or primitiveTrueLit primitiveTrueLit p ()
trueOrFalse = Or primitiveTrueLit primitiveFalseLit p ()
falseOrTrue = Or primitiveFalseLit primitiveTrueLit p ()
falseOrFalse = Or primitiveFalseLit primitiveFalseLit p ()

test_or = do
    PrimitiveVal (BoolVal x) <- eval trueOrTrue emptyEnvironment
    assertEqual x True
    PrimitiveVal (BoolVal x) <- eval trueOrFalse emptyEnvironment
    assertEqual x True
    PrimitiveVal (BoolVal x) <- eval falseOrTrue emptyEnvironment
    assertEqual x True
    PrimitiveVal (BoolVal x) <- eval falseOrFalse emptyEnvironment
    assertEqual x False


-- If

ifTrue = If primitiveTrueLit primitiveTrueLit primitiveFalseLit p ()
ifFalse = If primitiveFalseLit primitiveTrueLit primitiveFalseLit p ()

test_if = do
    PrimitiveVal (BoolVal x) <- eval ifTrue emptyEnvironment
    assertEqual x True
    PrimitiveVal (BoolVal x) <- eval ifFalse emptyEnvironment
    assertEqual x False
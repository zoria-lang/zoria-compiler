module TypeChecker where
  -- TODO: think about (maybe) changing the naming in Syntax.hs:
  -- TypeVariable, TypeVar, Identifier
  
  -- TODO: split TypeChecker.hs into smaller files

import Syntax
import Utility
import TypeChecker.Inference
import TypeChecker.Algorithm

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set


import Control.Monad.Except
import Control.Monad.State

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
                    
-- Should fail with unification error BoolT vs. IntT
testArray = Array [testPrimitiveBool, testPrimitiveInt] testPosition ()

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
                   testBlock, -- pass
                   testArray -- fail
                  ]


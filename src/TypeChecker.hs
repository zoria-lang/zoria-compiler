module TypeChecker where
  -- TODO: think about (maybe) changing the naming in Syntax.hs:
  -- TypeVariable, TypeVar, Identifier

  -- TODO: Better error handling (with position)
  
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
-- Bool
testPrimitiveBool = (Primitive (BoolLit False) testPosition ())
-- Int
testPrimitiveInt = Primitive (IntLit 6) testPosition ()
testLambdaVarToUnit = Lambda testVarPattern
                             (Primitive (UnitLit) testPosition ())
                             Nothing
                             testPosition
                             ()
-- a -> Int
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
-- Int -> Bool
testLambdaConst = Lambda (ConstPattern (IntLit 6) testPosition ()) 
                         testPrimitiveBool
                         -- (Var testIdX testPosition ()) 
                         Nothing 
                         testPosition
                         ()
-- Should fail with type unification (Bool -> Bool) u (Int -> Bool)
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
-- Bool
testLetVarPattern = LetIn (LetDef (Definition testVarPattern Nothing testPrimitiveInt testPosition))
                          (App testLambdaConst (Var testIdX testPosition ()) ())
                          ()

-- Bool
testAnd = And testLetVarPattern (App testLambdaConst testPrimitiveInt ()) testPosition ()

-- Should fail with type unification on comparing left and right
testOr = Or testLetVarPattern testPrimitiveInt testPosition ()

-- Int -> Bool
testAnnotated = AnnotatedExpr (testLambdaConst) 
                              (TypeSig [] (FunctionType (PrimitiveType IntT) (PrimitiveType BoolT))) 
                              testPosition 
                              ()

-- (Bool, Int)
testTuple1 = Tuple [testPrimitiveBool, testPrimitiveInt] testPosition ()
-- (Bool, Int, (Bool, Int))
testTuple2 = Tuple [testPrimitiveBool, testPrimitiveInt, testTuple1] testPosition ()

-- Int
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

testTuplePattern = TuplePattern [testVarPattern,
                                 ConstPattern (IntLit 6) testPosition ()] 
                                testPosition
                                ()

-- ((Int -> b), Int) -> b
testLambdaTuple = Lambda testTuplePattern
                         (App (Var testIdX testPosition ())
                              testPrimitiveInt
                              ())
                         Nothing
                         testPosition
                         ()

-- (Int -> Bool)
testMatchConst = Match testPrimitiveInt
                       [MatchCase (ConstPattern (IntLit 6) testPosition ()) testPrimitiveBool,
                        MatchCase (WildcardPattern testPosition ()) testPrimitiveBool]
                       testPosition
                       ()

testTuplePattern2 = TuplePattern [testVarPattern,
                                  (VarPattern (Identifier (T.pack "g")) testPosition ())]
                                 testPosition
                                 ()
-- (Bool, Int)
testLetTuplePattern = LetIn (LetDef (Definition testTuplePattern2 Nothing testTuple1 testPosition))
                          (Tuple [Var testIdX testPosition (),
                                  Var (Identifier (T.pack "g")) testPosition ()]
                                 testPosition
                                 ())
                          ()
                         
test :: Show a => Expr a -> IO ()
test expr =
    let (res, _) = runInference (inferExpr emptyEnv expr)
    in case res of
         Left err  ->  putStrLn $ "error: " ++ err
         Right t   ->  putStrLn $ show t


main :: IO ()
main = mapM_ test [testPrimitiveInt, -- pass 
                   testLambdaVar, -- pass
                   testLambdaVarOccur, -- fail
                   testLambdaConst, -- pass
                   testLambdaTuple, -- pass
                   testIf, -- fail
                   testApp, -- fail
                   testLetVarPattern, -- pass
                   testAnd, -- pass
                   testOr, -- fail
                   testAnnotated, -- pass
                   testTuple1, -- pass
                   testTuple2, -- pass
                   testBlock, -- pass
                   testArray, -- fail
                   testMatchConst,
                   testLetTuplePattern
                  ]

topLevelTypeDef = TypeDef $ TDef (TypeName (T.pack "TestType"))
                                 []
                                 Nothing
                                 [TypeCase (ConstructorName (T.pack "TestConstr")) 
                                           [PrimitiveType IntT, PrimitiveType BoolT]
                                           testPosition]
                                 testPosition

topLevelLetDef1 = TopLevelLet (LetDef (Definition (VarPattern (Identifier (T.pack "ignore")) testPosition ())
                                    Nothing 
                                    testLambdaVarToUnit 
                                    testPosition))
topLevelLetDef2 = TopLevelLet ((LetDef (Definition 
                                     testVarPattern
                                     Nothing
                                     testPrimitiveInt
                                     testPosition)))

topLevelLetDef3 = TopLevelLet ((LetDef (Definition (VarPattern (Identifier (T.pack "constructed")) testPosition ())
                                     Nothing
                                     (App (App (Constructor (ConstructorName (T.pack "TestConstr")) testPosition ()) testPrimitiveInt ())
                                          testPrimitiveBool ())
                                     testPosition)))

testProgram = Program (Module (ModuleId [] (ModName (T.pack"testMod")))
                              "./test"
                              []
                              Nothing
                              [topLevelTypeDef, topLevelLetDef1, topLevelLetDef2, topLevelLetDef3])


typecheckProgram :: Program a -> IO (Either [String] Environment)
typecheckProgram program = typecheckModule $ programRoot program


typecheckModule :: Module a -> IO (Either [String] Environment)
typecheckModule (Module id _ imports exports defs) = do
    resImports <- typecheckImports imports
    case resImports of
        Left errs -> return $ Left errs
        Right envImports -> 
            -- dont print, imports will print out from theirs typecheckModule
            let resDefs = typecheckDefs defs envImports
            in case resDefs of
                Left errs -> do
                    putStrLn $ "Failed loading module " ++ (T.unpack (unpackModName $ moduleName id))
                    mapM_ putStrLn errs
                    return $ Left errs
                Right envDefs -> return $ Right envDefs
    where
        unpackModName :: ModName -> T.Text
        unpackModName (ModName name) = name

typecheckImports :: [Import a] -> IO (Either [String] Environment)
typecheckImports [] = return $ Right emptyEnv
typecheckImports [i] = do
    resI <- typecheckModule (importMod i)
    return $ case resI of
        Left errs -> Left errs
        Right env -> Right env
typecheckImports (i:imports) = do
    resI <- typecheckModule (importMod i)
    case resI of
        Left errs -> return $ Left errs
        Right iEnv -> do
            resImports <- typecheckImports imports
            case resImports of
                Left errs -> return $ Left errs
                Right importsEnv -> return $ Right (mergeEnvs iEnv importsEnv)


typecheckDefs :: [TopLevelDef a] -> Environment -> Either [String] Environment
typecheckDefs [def] env = 
    case def of
        TopLevelLet (LetDef letDef) -> 
            let res = typecheckTopLevelLet letDef env
            in handleResult res
        AliasDef alias -> 
            let res = typecheckAlias alias env
            in handleResult res
        _ -> undefined
    where
        handleResult :: Either String Environment -> Either [String] Environment
        handleResult res = 
            case res of
                Left err -> Left [err]
                Right env' -> Right env'

-- TODO: this code is a mess
typecheckDefs (d:defs) env = 
    case d of
        TopLevelLet (LetDef letDef) ->
            let resHead = typecheckTopLevelLet letDef env
            in handleTail defs env resHead
    
        AliasDef alias ->
            let resHead = typecheckAlias alias env
            in handleTail defs env resHead
        TypeDef typeDef -> 
            let resHead = typecheckTypeDef typeDef env
            in handleTail defs env resHead
        _ -> undefined
    where
        handleTail :: [TopLevelDef a] -> 
                      Environment -> 
                      (Either String Environment) ->
                      (Either [String] Environment)
        handleTail defs env resHead = 
            case resHead of
                Right envHead -> 
                    let env' = (mergeEnvs env envHead)
                        resTail = (typecheckDefs defs env') 
                    in case resTail of
                        Right envTail -> Right (mergeEnvs env' envTail)
                        Left errTail -> Left errTail 
                -- if Def "d" didn't get to environment because of error
                -- then typechecking the tail ("defs") may lead to misleading
                -- errors like "unbound variable"
                -- TODO: how to tackle printing out multiple errors?
                --       for now it's going to be only the first encountered one
                --       (at least it would be reliable)
                Left err -> Left [err]
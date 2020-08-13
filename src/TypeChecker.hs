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

import System.Exit (exitFailure)
import Control.Monad.Except
import Control.Monad.State

typecheckProgram :: Program a -> IO (Environment)
typecheckProgram program = do
    result <- typecheckModule $ programRoot program
    case result of
        Left err -> exitFailure
        Right env -> return env


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
typecheckDefs [] env = Right emptyEnv
typecheckDefs [def] env = 
    case def of
        TopLevelLet (LetDef letDef) -> 
            let res = typecheckTopLevelLet letDef env
            in handleResult res
        AliasDef alias -> 
            let res = typecheckAlias alias env
            in handleResult res
        TypeDef typeDef ->
            let res = typecheckTypeDef typeDef env
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
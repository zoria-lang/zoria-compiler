module Main where

import Parser
import TypeChecker
import qualified Data.Text.IO as T
import PrettyPrint (prettyPrint)
import qualified Evaluator as Eval

main :: IO ()
main = parseProgram >>= typecheckProgram >>= putStrLn . show

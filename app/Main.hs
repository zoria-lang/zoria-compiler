module Main where

import Parser
import qualified Data.Text.IO as T
import PrettyPrint (prettyPrint)
import qualified Evaluator as Eval

main :: IO ()
main = parseProgram >>= T.putStrLn . prettyPrint 0

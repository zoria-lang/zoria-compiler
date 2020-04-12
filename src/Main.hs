module Main where

import GetOpt
import Parser
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = parseProgram >>= putStrLn . show
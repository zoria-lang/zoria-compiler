module Main where

import GetOpt
import Parser
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Text.IO as T
import PrettyPrint (prettyPrint)


main :: IO ()
main = parseProgram >>= T.putStrLn . prettyPrint 0
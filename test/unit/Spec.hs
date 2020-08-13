module Main where

import           Test.Tasty
import qualified GetOpt

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [GetOpt.tests]

{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Helpers where

import           Test.Framework

data TestValue = Expected | Received deriving (Show, Eq)

assertEqualLeft :: (Eq a, Show a) => Either a b -> Either a c -> IO ()
assertEqualLeft a b = assertEqual (Expected <$ a) (Received <$ b)

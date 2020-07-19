{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Helpers where

import           Test.Framework
import           Control.Arrow                  ( left )

assertEqualLeft :: (Eq a, Show a) => Either a b -> Either a c -> IO ()
assertEqualLeft a b = assertEqual ("expected" <$ a) ("actual" <$ b)

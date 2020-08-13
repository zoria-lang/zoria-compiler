{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

import {-@ HTF_TESTS @-} GetOptTests
import {-@ HTF_TESTS @-} Test.EvaluatorTests.EvalTests

main :: IO ()
main = htfMain htf_importedTests
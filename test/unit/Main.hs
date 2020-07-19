{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Test.GetOpt
import {-@ HTF_TESTS @-} Test.Parser.Module
import {-@ HTF_TESTS @-} Test.Parser.Common
import {-@ HTF_TESTS @-} Test.Parser.Identifier

main :: IO ()
main = htfMain htf_importedTests
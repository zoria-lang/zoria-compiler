{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserTests.Module
    ( htf_thisModulesTests
    )
where

import           Test.Framework
import           Helpers
import           Parser                         ( runParser )
import           Parser.Module

{- 
test_emptyFileFails = assertEqualLeft expected result
  where
    result   = runParser module' "Main.zo" ""
    expected = Left "xd"
 -}
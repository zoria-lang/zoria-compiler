{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserTests.Module
    ( htf_thisModulesTests
    )
where

import           Test.Framework
import           Helpers
import           ParserTests.Helpers
import           Parser                         ( runParser )
import           Parser.Module

test_emptyFileFails = assertEqualLeft expected result
  where
    result   = runParser module' "file" ""
    expected = makePrettyError
        "file"
        ""
        "unexpected end of input\nexpecting keyword 'module'"
        (1, 1)

test_noModuleNameFail = assertEqualLeft expected result
  where
    input    = "module "
    result   = runParser module' "file" input
    expected = makePrettyError
        "file"
        input
        "unexpected end of input\nexpecting module name"
        (1, 8)

{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Module
    ( htf_thisModulesTests
    )
where

import           Test.Framework
import           Test.Helpers
import           Test.Parser.Helpers
import           Parser.Module
import qualified Syntax                        as Ast

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

test_simpleModuleName = assertEqual expected result
  where
    result   = runParser moduleName "" "Foo"
    expected = Right $ Ast.ModuleId [] $ Ast.ModName "Foo"

test_qualifiedModuleName = assertEqual expected result
  where
    result   = runParser moduleName "" "Foo/Bar/FizzBuzz"
    expected = Right $ Ast.ModuleId (Ast.ModName <$> ["Foo", "Bar"])
                                    (Ast.ModName "FizzBuzz")

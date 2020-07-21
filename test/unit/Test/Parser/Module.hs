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
import           Parser.RawAst
import           Data.Text                      ( Text )
import qualified Syntax                        as Ast


simpleModuleName :: Text -> Ast.ModuleId
simpleModuleName name = Ast.ModuleId [] (Ast.ModName name)


test_moduleHeaderTests = do
    noModuleNameFail
    commentBeforeModuleName
  where
    noModuleNameFail = assertEqualLeft expected result
      where
        input    = "module "
        result   = runParser moduleHeader "file" input
        expected = makePrettyError
            "file"
            input
            "unexpected end of input\nexpecting module name"
            (1, 8)

    commentBeforeModuleName = assertEqual expected result
      where
        result   = runParser moduleHeader "" "module {# comment #} Main"
        expected = Right (Ast.ModuleId [] (Ast.ModName "Main"), Everything)

test_moduleNameTests = do
    simpleModuleName
    moduleNameErrorMessage
    qualifiedModuleName
    commentBetweenModuleQualifiers
  where
    simpleModuleName = assertEqual expected result
      where
        result   = runParser qualifiedModuleId "" "Foo"
        expected = Right $ Ast.ModuleId [] $ Ast.ModName "Foo"

    moduleNameErrorMessage = assertEqualLeft expected result
      where
        input    = "lowercase"
        file     = "file"
        result   = runParser moduleName file input
        expected = makePrettyError file
                                   input
                                   "unexpected 'l'\nexpecting module name"
                                   (1, 1)

    qualifiedModuleName = assertEqual expected result
      where
        result   = runParser qualifiedModuleId "" "Foo\\Bar\\FizzBuzz"
        expected = Right $ Ast.ModuleId (Ast.ModName <$> ["Foo", "Bar"])
                                        (Ast.ModName "FizzBuzz")

    commentBetweenModuleQualifiers = assertEqual expected result
      where
        result   = runParser qualifiedModuleId "" "Foo\\{# foo #}Bar{# bar #}"
        expected = Right $ Ast.ModuleId [Ast.ModName "Foo"] (Ast.ModName "Bar")

test_fullModuleTests = do
    emptyFileFails
    moduleSimpleImports
  where
    emptyFileFails = assertEqualLeft expected result
      where
        result   = runParser module' "file" ""
        expected = makePrettyError
            "file"
            ""
            "unexpected end of input\nexpecting keyword 'module'"
            (1, 1)

    moduleSimpleImports = assertEqual expected result
      where
        file     = "Main.zo"
        input    = "module Main import FooBar"
        result   = runParser module' file input
        expected = Right mockModule { modId      = simpleModuleName "Main"
                                    , modExports = Everything
                                    , modImports = imports
                                    }
        imports =
            [ located
                  (Import (simpleModuleName "FooBar") Nothing Everything)
                  (file, 12)
            ]

test_importListTests = do
    noImportList
    emptyImportList
  where
    noImportList = assertEqual (Right Everything) result
        where result = runParser importList "" ""

    emptyImportList = assertEqual (Right (Specified [])) result
        where result = runParser importList "" "[]"


test_importListNonEmpty = nonImplementedTest

test_moduleEmptyExport = nonImplementedTest

test_moduleSimpleExport = nonImplementedTest

test_moduleExportOperator = nonImplementedTest

test_multipleImports = nonImplementedTest

test_moduleExportEverythingFromType = nonImplementedTest

test_moduleExportTypeWithConstructors = nonImplementedTest

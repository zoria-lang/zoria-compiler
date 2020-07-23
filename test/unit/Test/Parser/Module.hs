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
import           Utility                        ( Position(..) )
import qualified Syntax                        as Ast


simpleModuleName :: Text -> Ast.ModuleId
simpleModuleName name = Ast.ModuleId [] (Ast.ModName name)

fileName :: FilePath
fileName = "file"


test_moduleHeaderTests = do
    noModuleNameFail
    commentBeforeModuleName
    moduleWithQualifiedName
    moduleEmptyExport
    moduleSimpleExport
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
        result = runParser moduleHeader "" "module {# comment #} Foo\\Main"
        expected =
            Right
                ( Ast.ModuleId [Ast.ModName "Foo"] (Ast.ModName "Main")
                , Everything
                )

    moduleWithQualifiedName = assertEqual expected result
      where
        result =
            runParser moduleHeader "" "module Foo\\Bar\\FooBar\\Fizz\\Buzz"
        expected = Right (modId, Everything)
        modId = Ast.ModuleId (Ast.ModName <$> ["Foo", "Bar", "FooBar", "Fizz"])
            $ Ast.ModName "Buzz"

    moduleEmptyExport = assertEqual expected result
      where
        result   = runParser moduleHeader fileName "module Main ()"
        expected = Right (Ast.ModuleId [] (Ast.ModName "Main"), Specified [])

    moduleSimpleExport = assertEqual expected result
      where
        result =
            runParser moduleHeader fileName "module Main (foo, bar, fizzBuzz)"
        expected =
            Right (Ast.ModuleId [] (Ast.ModName "Main"), Specified exports)
        export name pos =
            Ast.Located pos (Ast.ImportedIdentifier $ Ast.Identifier name)
        exports =
            [ export "foo"      (Position 13 fileName)
            , export "bar"      (Position 18 fileName)
            , export "fizzBuzz" (Position 23 fileName)
            ]

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
    importListSingleSimple
    importManySimple
  where
    importedIdentifier offset =
        Ast.Located (Position offset fileName)
            . Ast.ImportedIdentifier
            . Ast.Identifier

    noImportList = assertEqual (Right Everything) result
        where result = runParser importList "" ""

    emptyImportList = assertEqual (Right (Specified [])) result
        where result = runParser importList "" "()"

    importListSingleSimple = assertEqual expected result
      where
        result   = runParser importList fileName "(something)"
        expected = Right $ Specified [importedIdentifier 1 "something"]

    importManySimple = assertEqual expected result
      where
        result   = runParser importList fileName "(a, b)"
        expected = Right
            $ Specified [importedIdentifier 1 "a", importedIdentifier 4 "b"]

test_moduleExportOperator = nonImplementedTest

test_multipleImports = nonImplementedTest

test_importOperator = nonImplementedTest

test_importType = nonImplementedTest

test_moduleExportEverythingFromType = nonImplementedTest

test_moduleExportTypeWithConstructors = nonImplementedTest

test_moduleExportNothingFromType = nonImplementedTest

test_moduleExportOperatorFromType = nonImplementedTest

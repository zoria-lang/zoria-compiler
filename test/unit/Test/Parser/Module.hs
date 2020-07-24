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

importElem :: Text -> Position -> Ast.Located Ast.ImportedValue
importElem name pos =
    Ast.Located pos (Ast.ImportedIdentifier $ Ast.Identifier name)


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
        exports =
            [ importElem "foo"      (Position 13 fileName)
            , importElem "bar"      (Position 18 fileName)
            , importElem "fizzBuzz" (Position 23 fileName)
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
    multipleImports
    qualifiedImport
    invalidQualifiedImport
  where
    emptyFileFails = assertEqualLeft expected result
      where
        result   = runParser module' fileName ""
        expected = makePrettyError
            fileName
            ""
            "unexpected end of input\nexpecting keyword 'module'"
            (1, 1)

    moduleSimpleImports = assertEqual expected result
      where
        input    = "module Main import FooBar"
        result   = runParser module' fileName input
        expected = Right mockModule { modId      = simpleModuleName "Main"
                                    , modExports = Everything
                                    , modImports = imports
                                    }
        imports =
            [ located
                  (Import (simpleModuleName "FooBar") Nothing Everything)
                  (fileName, 19)
            ]

    multipleImports = assertEqual expected result
      where
        result = runParser
            module'
            fileName
            "module Main import Foo () import Bar import Fizz (buzz)"
        expected = Right mockModule { modId      = simpleModuleName "Main"
                                    , modExports = Everything
                                    , modImports = imports
                                    }
        imports =
            [ located
                (Import (simpleModuleName "Foo") Nothing (Specified []))
                (fileName, 19)
            , located (Import (simpleModuleName "Bar") Nothing Everything)
                      (fileName, 33)
            , located
                (Import (simpleModuleName "Fizz") Nothing (Specified [buzz]))
                (fileName, 44)
            ]
            where buzz = importElem "buzz" (Position 50 fileName)

    invalidQualifiedImport = assertEqualLeft expected result
      where
        input    = "module Main\nimport Foo.Bar"
        result   = runParser (untilEof module') fileName input
        expected = makePrettyError
            fileName
            input
            "unexpected '.'\nexpecting '\\', end of input, keyword 'as', keyword 'import', or list of identifiers"
            (2, 11)

    qualifiedImport = assertEqual expected result
      where
        result =
            runParser module' fileName "module Main import Foo\\Bar (foobar)"
        expected = Right mockModule { modId      = simpleModuleName "Main"
                                    , modExports = Everything
                                    , modImports = imports
                                    }
        imports =
            [located (Import modId Nothing (Specified [foobar])) (fileName, 19)]
          where
            modId  = Ast.ModuleId [Ast.ModName "Foo"] (Ast.ModName "Bar")
            foobar = importElem "foobar" (Position 28 fileName)

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

test_importAlias = notImplementedTest

test_moduleExportOperator = notImplementedTest

test_importOperator = notImplementedTest

test_importType = notImplementedTest

test_moduleExportEverythingFromType = notImplementedTest

test_moduleExportTypeWithConstructors = notImplementedTest

test_moduleExportNothingFromType = notImplementedTest

test_moduleExportOperatorFromType = notImplementedTest

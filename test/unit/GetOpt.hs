{-# LANGUAGE OverloadedStrings #-}
module GetOpt
    ( tests
    )
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           GetOpt.Internal
import           Utility
import           Control.Arrow                  ( left )

tests :: TestTree
tests = testGroup
    "GetOpt"
    [ testCase "No options"                  noOptions
    , testCase "Input file only"             onlyInput
    , testCase "Specified output"            withOutput
    , testCase "Specified module path"       withModulePath
    , testCase "Specified many module paths" withManyModulePaths
    , testGroup
        "Module path parsing"
        [ testCase "No separator" noModulePathSeparator
        , testCase "Empty path"   emptyPath
        , testCase "Empty name"   emptyName
        , testCase "Simple path"  simpleModulePath
        ]
    ]

args :: String -> IO [String]
args = return . words

noOptions = assertThrows (parseOptions $ args "")

onlyInput = do
    result <- parseOptions (args "abc")
    defaultOptions { optInputs = ["abc"] } @=? result

testParseOptions flags correct = do
    result <- parseOptions $ args flags
    correct @=? result

withOutput = do
    testParseOptions "in -o out"       correct
    testParseOptions "in --output out" correct
    where correct = defaultOptions { optOutput = "out", optInputs = ["in"] }

withModulePath = do
    testParseOptions "in -m Mod:abc/123"       correct
    testParseOptions "in --module=Mod:abc/123" correct
  where
    correct = defaultOptions { optInputs  = ["in"]
                             , optModules = [ModulePath "Mod" "abc/123"]
                             }

withManyModulePaths = (expected @=?) =<< result
  where
    expected = defaultOptions
        { optModules = [ModulePath "Bar" "bar", ModulePath "Foo" "foo"]
        , optInputs  = ["in"]
        , optOutput  = "out.exe"
        }
    result = parseOptions $ args "in -m Foo:foo --module=Bar:bar -o out.exe"

noModulePathSeparator = error @=? result
  where
    result = left show $ addModuleOpt "FooBar" defaultOptions
    error  = Left "user error ('FooBar': missing the ':' separator!)"

emptyPath = error @=? result
  where
    result = left show $ addModuleOpt "Foo:" defaultOptions
    error  = Left "user error ('Foo:': module path is empty!)"

emptyName = error @=? result
  where
    result = left show $ addModuleOpt ":Bar" defaultOptions
    error  = Left "user error (':Bar': module name is empty!)"

simpleModulePath = correct @=? result
  where
    correct = Right $ defaultOptions { optModules = [ModulePath "Foo" "Bar"] }
    result  = addModuleOpt "Foo:Bar" defaultOptions


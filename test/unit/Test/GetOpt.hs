{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.GetOpt
    ( htf_thisModulesTests
    )
where

import           Test.Framework
import           GetOpt.Internal

testOptions :: Options
testOptions = defaultOptions

fmapLeft :: (a -> c) -> Either a b -> Either c b
fmapLeft f (Left  a) = Left (f a)
fmapLeft _ (Right x) = Right x

args :: String -> IO [String]
args = return . words

test_parseOptionsTests = do
    parseNoOptions
    parseOnlyInputFile
    parseOutput
    parseModulePath
    parseManyModulePaths
    parseAllFlags
  where
    parseNoOptions     = assertThrowsSomeIO (parseOptions $ args "")

    parseOnlyInputFile = assertEqual correct <$> result
      where
        result  = parseOptions $ args "abc.exe"
        correct = testOptions { optInputs = ["abc.exe"] }

    parseOutput = do
        assertEqual correct <$> result short
        assertEqual correct <$> result full
      where
        result flag = parseOptions $ args ("input.zo " ++ flag ++ " abc.exe")
        short   = "-o"
        full    = "--output"
        correct = testOptions { optOutput = "abc.exe" }

    parseModulePath = do
        assertEqual correct <$> result short
        assertEqual correct <$> result full
      where
        result flag =
            parseOptions $ args ("input.zo Mod" ++ flag ++ ":abc/123")
        short   = "-m"
        full    = "--module"
        correct = testOptions { optModules = [ModulePath "Mod" "abc/123"] }

    parseManyModulePaths = assertEqual correct <$> result
      where
        result  = parseOptions $ args "in.zo -m A:a --module B:b"
        correct = testOptions
            { optModules = [ModulePath "A" "a", ModulePath "B" "b"]
            }

    parseAllFlags = assertEqual correct <$> result
      where
        result  = parseOptions $ args "in.zo -m A:a --module B:b -o out.exe"
        correct = testOptions
            { optModules = [ModulePath "A" "a", ModulePath "B" "b"]
            , optInputs  = ["in.zo"]
            , optOutput  = "out.exe"
            }

test_addModTests = do
    noModulePathSepartor
    emptyModulePath
    emptyModuleName
    simpleExample
    multipleColons
    complexPath
  where
    noModulePathSepartor = assertEqual error result
      where
        result = fmapLeft show $ addModuleOpt "FooBar" testOptions
        error  = Left "user error ('FooBar': missing the ':' separator!)"

    emptyModulePath = assertEqual error result
      where
        result = fmapLeft show $ addModuleOpt "Foo:" testOptions
        error  = Left "user error ('Foo:': module path is empty!)"

    emptyModuleName = assertEqual error result
      where
        result = fmapLeft show $ addModuleOpt ":Bar" testOptions
        error  = Left "user error (':Bar': module name is empty!)"

    simpleExample = assertEqual correct result
      where
        result  = addModuleOpt "Foo:Bar" testOptions
        correct = Right $ testOptions { optModules = [ModulePath "Foo" "Bar"] }

    multipleColons = assertEqual correct result
      where
        result = addModuleOpt "Foo::Bar:Buzz" testOptions
        correct =
            Right $ testOptions { optModules = [ModulePath "Foo" ":Bar:Buzz"] }

    complexPath = assertEqual correct result
      where
        result = addModuleOpt "Foo:foo/bar/a" testOptions
        correct =
            Right $ testOptions { optModules = [ModulePath "Foo" "foo/bar/a"] }

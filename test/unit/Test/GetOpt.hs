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

test_parseNoOptions = assertThrowsSomeIO (parseOptions $ args "")

test_parseOnlyInputFile = assertEqual correct <$> result
  where
    result  = parseOptions $ args "abc.exe"
    correct = testOptions { optInputs = ["abc.exe"] }

test_parseOutput = do
    assertEqual correct <$> result short
    assertEqual correct <$> result full
  where
    result flag = parseOptions $ args ("input.zo " ++ flag ++ " abc.exe")
    short   = "-o"
    full    = "--output"
    correct = testOptions { optOutput = "abc.exe" }

test_parseModulePath = do
    assertEqual correct <$> result short
    assertEqual correct <$> result full
  where
    result flag = parseOptions $ args ("input.zo Mod" ++ flag ++ ":abc/123")
    short   = "-m"
    full    = "--module"
    correct = testOptions { optModules = [ModulePath "Mod" "abc/123"] }

test_parseManyModulePaths = assertEqual correct <$> result
  where
    result = parseOptions $ args "in.zo -m A:a --module B:b"
    correct =
        testOptions { optModules = [ModulePath "A" "a", ModulePath "B" "b"] }

test_parseAllFlags = assertEqual correct <$> result
  where
    result  = parseOptions $ args "in.zo -m A:a --module B:b -o out.exe"
    correct = testOptions
        { optModules = [ModulePath "A" "a", ModulePath "B" "b"]
        , optInputs  = ["in.zo"]
        , optOutput  = "out.exe"
        }

test_addMod_noModulePathSepartor = assertEqual error result
  where
    result = fmapLeft show $ addModuleOpt "FooBar" testOptions
    error  = Left "user error ('FooBar': missing the ':' separator!)"

test_addMod_emptyModulePath = assertEqual error result
  where
    result = fmapLeft show $ addModuleOpt "Foo:" testOptions
    error  = Left "user error ('Foo:': module path is empty!)"

test_addMod_emptyModuleName = assertEqual error result
  where
    result = fmapLeft show $ addModuleOpt ":Bar" testOptions
    error  = Left "user error (':Bar': module name is empty!)"

test_addMod_simpleExample = assertEqual correct result
  where
    result  = addModuleOpt "Foo:Bar" testOptions
    correct = Right $ testOptions { optModules = [ModulePath "Foo" "Bar"] }

test_addMod_multipleColons = assertEqual correct result
  where
    result = addModuleOpt "Foo::Bar:Buzz" testOptions
    correct =
        Right $ testOptions { optModules = [ModulePath "Foo" ":Bar:Buzz"] }

test_addMod_complexPath = assertEqual correct result
  where
    result = addModuleOpt "Foo:foo/bar/a" testOptions
    correct =
        Right $ testOptions { optModules = [ModulePath "Foo" "foo/bar/a"] }

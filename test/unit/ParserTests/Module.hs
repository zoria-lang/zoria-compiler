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
import           Data.List                      ( intercalate )

errorMsg :: [String] -> Either String a
errorMsg = Left . intercalate "\n"


test_emptyFileFails = assertEqualLeft expected result
  where
    result   = runParser module' "file" ""
    expected = makePrettyError
        "file"
        ""
        "unexpected end of input\nexpecting keyword 'module'"
        (1, 1)

test_noModuleName = assertBool False

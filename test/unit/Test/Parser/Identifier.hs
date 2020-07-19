{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Identifier
    ( htf_thisModulesTests
    )
where

import           Test.Framework
import           Test.Helpers
import           Test.Parser.Helpers
import           Parser.Identifier

test_singleLetterUppercaseName =
    assertEqual (Right "A") (runUntilEof uppercaseIdentifier "A ")

test_emptyStringUppercaseNameShouldFail = assertEqualLeft expected result
  where
    result   = runParser uppercaseIdentifier "file" ""
    expected = makePrettyError
        "file"
        ""
        "unexpected end of input\nexpecting uppercase letter"
        (1, 1)

test_invalidNumberUppercaseName = assertEqualLeft expected result
  where
    input    = "2137"
    result   = runParser uppercaseIdentifier "file" input
    expected = makePrettyError "file"
                               input
                               "unexpected '2'\nexpecting uppercase letter"
                               (1, 1)

test_uppercaseNameWithDigits = assertEqual
    (Right "F2o1o3b7aR")
    (runUntilEof uppercaseIdentifier "F2o1o3b7aR")

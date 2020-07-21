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
import           Parser.Common                  ( symbol )
import           Control.Applicative            ( Alternative((<|>)) )
import           Control.Monad                  ( void )

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

test_uppercaseNameWithApostrophe =
    assertEqual (Right "Foo'") (runUntilEof uppercaseIdentifier "Foo'")

test_uppercaseNameWithUnderscore =
    assertEqual (Right "Foo_Bar") (runUntilEof uppercaseIdentifier "Foo_Bar")

test_keywordRejectsSuffix = assertLeft $ runParser (keyword "a") "" "ab"

test_keywordHasNoNumericSuffix = assertLeft $ runParser (keyword "a") "" "a6"

test_keywordEatsWhitespace =
    assertRight $ runUntilEof (keyword "a") "a  \n {# comment #}  \n # comment "

test_keywordDoesntEatWithoutFullMatch =
    assertRight $ runUntilEof (keyword "abc" <|> void (symbol "abcd")) "abcd"

test_requireWhitespaceBetweenKeywords =
    assertLeft $ runParser (keyword "a" >> keyword "b") "" "ab"

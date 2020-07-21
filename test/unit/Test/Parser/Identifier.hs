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

test_uppercaseIdentifierTests = do
    singleLetterUppercaseName
    emptyStringUppercaseNameShouldFail
    invalidNumberUppercaseName
    uppercaseNameWithDigits
    uppercaseNameWithApostrophe
    uppercaseNameWithUnderscore
  where
    singleLetterUppercaseName =
        assertEqual (Right "A") (runUntilEof uppercaseIdentifier "A ")

    emptyStringUppercaseNameShouldFail = assertEqualLeft expected result
      where
        result   = runParser uppercaseIdentifier "file" ""
        expected = makePrettyError
            "file"
            ""
            "unexpected end of input\nexpecting uppercase letter"
            (1, 1)

    invalidNumberUppercaseName = assertEqualLeft expected result
      where
        input    = "2137"
        result   = runParser uppercaseIdentifier "file" input
        expected = makePrettyError
            "file"
            input
            "unexpected '2'\nexpecting uppercase letter"
            (1, 1)

    uppercaseNameWithDigits = assertEqual
        (Right "F2o1o3b7aR")
        (runUntilEof uppercaseIdentifier "F2o1o3b7aR")

    uppercaseNameWithApostrophe =
        assertEqual (Right "Foo'") (runUntilEof uppercaseIdentifier "Foo'")

    uppercaseNameWithUnderscore = assertEqual
        (Right "Foo_Bar")
        (runUntilEof uppercaseIdentifier "Foo_Bar")

test_keywordTests = do
    keywordRejectsSuffix
    keywordHasNoNumericSuffix
    keywordEatsWhitespace
    keywordDoesntEatWithoutFullMatch
    requireWhitespaceBetweenKeywords
  where
    keywordRejectsSuffix      = assertLeft $ runParser (keyword "a") "" "ab"

    keywordHasNoNumericSuffix = assertLeft $ runParser (keyword "a") "" "a6"

    keywordEatsWhitespace     = assertRight
        $ runUntilEof (keyword "a") "a  \n {# comment #}  \n # comment "

    keywordDoesntEatWithoutFullMatch = assertRight
        $ runUntilEof (keyword "abc" <|> void (symbol "abcd")) "abcd"

    requireWhitespaceBetweenKeywords =
        assertLeft $ runParser (keyword "a" >> keyword "b") "" "ab"

{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Common
    ( htf_thisModulesTests
    )
where

import           Test.Framework
import           Parser.Common
import           Data.Text                      ( Text )
import           Test.Parser.Helpers
import           Test.Helpers

runWhitespace :: Text -> Either String ()
runWhitespace = runUntilEof whitespace

runSymbol :: Text -> Text -> Either String Text
runSymbol = runUntilEof . symbol

test_emptyTextWhitespace = assertRight (runWhitespace "")

test_variousWhitespace = assertRight (runWhitespace " \r\n\n\t   ")

test_lineComment = assertRight (runWhitespace "# this is a comment")

test_manyLineComments = assertRight (runWhitespace comments)
    where comments = "#comment\n# next comment\n #final comment"

test_blockComment = assertRight (runWhitespace "{# 1\n2 #}")

test_blockCommentNotTerminated = assertLeft (runWhitespace "{#")

test_bracketInsideBlockComment = assertRight (runWhitespace "{# {} #}")

test_nestedBlockComments = assertRight (runWhitespace "{# {##} # {##} #}")

test_singleSymbol = assertEqual (Right "symbol") (runSymbol "symbol" "symbol")

test_emptyTextSymbol = assertLeft $ runSymbol "a" ""

test_symbolConsumesOnlyTrailingWhitespace = assertEqual (Right "b") result
    where result = runParser (symbol "a" >> symbol "b") "" "a b"

test_emptyList = assertEqual (Right []) result
  where
    listParser = list' "[" (symbol "A") "]" ","
    result     = runParser listParser "" "[]"

test_listWithNoBeginningAndEnd = assertEqual expected result
  where
    expected   = Right ["A", "A", "A", "A", "A"]
    listParser = list' "" (symbol "A") "" "/"
    result     = runParser listParser "" "A/A /A/ A  /  A"

test_emptyNonEmptyList = assertEqualLeft expected result
  where
    input = "[]"
    expected =
        makePrettyError "file" input "unexpected ']'\nexpecting 'A'" (1, 2)
    listParser = list1' "[" (symbol "A") "," "]"
    result     = runParser listParser "file" input

test_emptyNonEmptyListWithoutStartAndEnd = assertEqualLeft expected result
  where
    expected = makePrettyError "file"
                               ""
                               "unexpected end of input\nexpecting 'A'"
                               (1, 1)
    listParser = list1 "" (symbol "A") "," ""
    result     = runParser listParser "file" ""

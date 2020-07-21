{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Common
    ( htf_thisModulesTests
    )
where

import           Test.Framework
import           Utility                        ( Position(..) )
import           Parser.Common
import           Data.Text                      ( Text )
import           Test.Parser.Helpers
import           Test.Helpers
import           Data.Functor                   ( ($>) )

runWhitespace :: Text -> Either String ()
runWhitespace = runUntilEof whitespace

runSymbol :: Text -> Text -> Either String Text
runSymbol = runUntilEof . symbol

test_whitespaceTests = do
    emptyTextWhitespace
    variousWhitespace
    lineComment
    manyLineComments
    blockComment
    blockCommentNotTerminated
    bracketInsideBlockComment
    nestedBlockComments
  where
    emptyTextWhitespace = assertRight (runWhitespace "")

    variousWhitespace   = assertRight (runWhitespace " \r\n\n\t   ")

    lineComment         = assertRight (runWhitespace "# this is a comment")

    manyLineComments    = assertRight (runWhitespace comments)
        where comments = "#comment\n# next comment\n #final comment"

    blockComment              = assertRight (runWhitespace "{# 1\n2 #}")

    blockCommentNotTerminated = assertLeft (runWhitespace "{#")

    bracketInsideBlockComment = assertRight (runWhitespace "{# {} #}")

    nestedBlockComments       = assertRight (runWhitespace "{# {##} # {##} #}")

test_symbolTests = do
    singleSymbol
    emptyTextSymbol
    symbolConsumesOnlyTrailingWhitespace
  where
    singleSymbol = assertEqual (Right "symbol") (runSymbol "symbol" "symbol")

    emptyTextSymbol = assertLeft $ runSymbol "a" ""

    symbolConsumesOnlyTrailingWhitespace = assertEqual (Right "b") result
        where result = runParser (symbol "a" >> symbol "b") "" "a b"

test_listTest = do
    emptyList
    listWithNoBeginningAndEnd
    emptyNonEmptyList
    emptyNonEmptyListWithoutStartAndEnd
  where
    emptyList = assertEqual (Right []) result
      where
        listParser = list' "[" (symbol "A") "]" ","
        result     = runParser listParser "" "[]"

    listWithNoBeginningAndEnd = assertEqual expected result
      where
        expected   = Right ["A", "A", "A", "A", "A"]
        listParser = list' "" (symbol "A") "" "/"
        result     = runParser listParser "" "A/A /A/ A  /  A"

    emptyNonEmptyList = assertEqualLeft expected result
      where
        input = "[]"
        expected =
            makePrettyError "file" input "unexpected ']'\nexpecting 'A'" (1, 2)
        listParser = list1' "[" (symbol "A") "," "]"
        result     = runParser listParser "file" input

    emptyNonEmptyListWithoutStartAndEnd = assertEqualLeft expected result
      where
        expected = makePrettyError "file"
                                   ""
                                   "unexpected end of input\nexpecting 'A'"
                                   (1, 1)
        listParser = list1 "" (symbol "A") "," ""
        result     = runParser listParser "file" ""

test_withPosTests = simplePosTest
  where
    simplePosTest = do
        assertEqual expected1 (parse "A" $ posParser (symbol "A"))
        assertEqual expected2
                    (parse "q   B" $ symbol "q" *> posParser (symbol "B"))
      where
        fileName = "FileName"
        posParser parser = withPos $ \pos -> parser $> pos
        parse input parser = runParser parser fileName input
        expected1 = Right $ Position 0 fileName
        expected2 = Right $ Position 4 fileName

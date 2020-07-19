{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserTests.Common
    ( htf_thisModulesTests
    )
where

import           Test.Framework
import           Parser                         ( runParser )
import           Parser.Common
import           Data.Text                      ( Text )

runUntilEof :: Parser a -> Text -> Either String a
runUntilEof p = runParser (untilEof p) ""

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

test_keywordRejectsSuffix = assertLeft $ runParser (keyword "a")  "" "ab"

test_keywordHasNoNumericSuffix = assertLeft $ runParser (keyword "a") "" "a6"

test_requireWhitespaceBetweenKeywords =
    assertLeft $ runParser (keyword "a" >> keyword "b") "" "ab"

{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserTests.Identifier
    ( htf_thisModulesTests
    )
where

import           Test.Framework
import           ParserTests.Helpers
import           Parser.Identifier

test_singleLetterUppercaseName =
    assertEqual (Right "A") (runUntilEof uppercaseIdentifier "A ")

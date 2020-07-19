{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Parser.Identifier
    ( htf_thisModulesTests
    )
where

import           Test.Framework
import           Test.Parser.Helpers
import           Parser.Identifier

test_singleLetterUppercaseName =
    assertEqual (Right "A") (runUntilEof uppercaseIdentifier "A ")

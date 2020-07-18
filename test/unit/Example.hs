{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Example (htf_thisModulesTests) where

import Test.Framework

test_example = assertBool True
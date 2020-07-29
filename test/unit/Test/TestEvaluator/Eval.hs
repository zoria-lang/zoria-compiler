{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.TestEvaluator.Eval
    ( htf_thisModulesTests
    )
where

import Test.Framework
import Syntax
import Utility
import Evaluator
import Evaluator.Environment
import Evaluator.ValueSyntax

p :: Position
p = Position 0 ""

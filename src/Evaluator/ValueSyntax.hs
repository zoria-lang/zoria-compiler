{-# LANGUAGE FlexibleInstances #-}

module Evaluator.ValueSyntax where

import qualified Data.Map                      as M
import qualified Data.Text                     as T

import Syntax

-- TODO: Think about Environment structure. How modules should be stored etc.

newtype Environment = Environment (M.Map T.Text Value)
    deriving Show

-- TODO: What the Value type should look like?

data PrimVal
    = IntVal Int
    -- ^ 64 bit signed integers 
    | CharVal Char
    -- ^ single unicode character
    | FloatVal Double
    -- ^ double precision floating number
    | StringVal T.Text
    -- ^ non-format string 
    | BoolVal Bool
    -- ^ boolean value
    | UnitVal
    -- ^ () value
  deriving (Show, Eq)

data Value
  = PrimitiveVal PrimVal
  | Procedure  (Pattern ()) (Expr ()) Environment
  | TupleVal [Value]
  | CustomVal ConstructorName [Value]
  | InternalFun ([Value] -> IO Value)
  deriving Show
-- TODO: Should constructor be stored as procedure, or as

instance Show ([Value] -> IO Value) where
    show _ = "InternalFun"
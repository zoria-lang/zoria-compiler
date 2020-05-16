module ValueSyntax where

import qualified Data.Map                      as M
import qualified Data.Text                     as T

import Syntax

newtype Environment = Environment (M.Map T.Text Value)
    deriving Show

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
  deriving Show
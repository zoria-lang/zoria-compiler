module ValueSyntax where

import qualified Data.Text as T

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
  deriving Show

data Value = PrimitiveVal PrimVal | TupleVal [Value]
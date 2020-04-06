module Utility where


data Position = Position
    { posOffset :: !Int
    , posFile   :: !FilePath
    }
  deriving (Show, Eq, Ord)
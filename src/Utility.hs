module Utility where


import Control.Monad (forM)
import Data.List (find)

data Position = Position
    { posOffset :: !Int
    , posFile   :: !FilePath
    }
  deriving Show


findM :: (Traversable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p xs = do
    xs' <- forM xs p'
    return $ snd <$> find fst xs'
  where
    p' x = do
        bool <- p x
        return (bool, x)

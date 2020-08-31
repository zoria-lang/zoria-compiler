module Utility where

import           Test.Tasty.HUnit
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad                  ( unless )
import           Data.Functor                   ( ($>) )

assertThrows :: IO a -> IO ()
assertThrows action = do
    thrown <- catch (action $> False) handler
    unless thrown $ assertFailure "Expected failure but there was no exception"
  where
    handler :: SomeException -> IO Bool
    handler _ = return True



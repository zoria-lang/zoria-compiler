module Main where

import GetOpt


main :: IO ()
main = do
    options <- getOptions
    print options
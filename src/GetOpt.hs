module GetOpt
    ( Options(..)
    , ModulePath(..)
    , getOptions
    )
where

import           GetOpt.Internal
import           System.Environment             ( getArgs )


getOptions :: IO Options
getOptions = parseOptions getArgs

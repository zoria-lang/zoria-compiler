module GetOpt where

import System.Console.GetOpt
import qualified Data.Text as T
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.Environment (getProgName)

data Arguments = Arguments
    { argsOptions :: Options
    , argsInput   :: FilePath
    }

data Options = Options
    { optOutput  :: FilePath
    , optModules :: [ModulePath]
    }

data ModulePath = ModulePath
    { optModName :: T.Text
    , optModPath :: FilePath
    }

defaultOptions :: Options
defaultOptions = Options
    { optModules = []
    , optOutput  = "a.out"
    }

addModuleOpt :: String -> Options -> Options
addModuleOpt modOpt opts = opts { optModules = modPath : modules }
  where
    modules = optModules opts
    modPath = undefined

options :: [OptDescr (Options -> IO Options)]
options = 
    [ Option "o" ["output"] 
        (ReqArg (\arg opts -> return opts { optOutput = arg }) "FILE")
        "Output file"
    , Option "V" ["version"] 
        (NoArg (\_ -> do
            putStrLn "Version 0.1.0.0" -- TODO: embed the version from *.cabal
            exitWith ExitSuccess
        ))
        "Print program version"
    , Option "h" ["help"]
        (NoArg (\_ -> do
            progName <- getProgName
            putStrLn (usageInfo progName options)
            exitWith ExitSuccess
        ))
        "Show help"
    , Option "m" ["module"]
        (ReqArg (\arg opts -> return $ addModuleOpt arg opts) "MODULE:PATH")
        "Extern module path"
    ]













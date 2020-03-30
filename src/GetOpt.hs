module GetOpt
    ( Options(..)
    , Arguments(..)
    , ModulePath(..)
    , getOptions
    )
where

import System.Console.GetOpt
import qualified Data.Text as T
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.Environment (getProgName, getArgs)

data Arguments = Arguments
    { argsOptions :: Options
    , argsInput   :: FilePath
    }
  deriving Show

data Options = Options
    { optOutput  :: FilePath
    , optModules :: [ModulePath]
    }
  deriving Show

data ModulePath = ModulePath
    { optModName :: T.Text
    , optModPath :: FilePath
    }
  deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optModules = []
    , optOutput  = "a.out"
    }

addModuleOpt :: String -> Options -> Either IOError Options
addModuleOpt modOpt opts = do
    modPath <- splitModOpt modOpt
    return $ opts { optModules = modPath : modules }
  where
    modules = optModules opts
    splitModOpt :: String -> Either IOError ModulePath
    splitModOpt str = case split str of
        Just ("", _) -> Left emptyNameErr
        Just (_, "") -> Left emptyPathErr
        Nothing -> Left sepErr
        Just (modName, modPath) -> Right $ ModulePath (T.pack modName) modPath
      where
        sepErr = userError $ '\'': str ++ "': missing the ':' separator!"
        emptyPathErr = userError $ '\'':str ++ "': module path is empty!"
        emptyNameErr = userError $ '\'':str ++ "': module name is empty!"
        split :: String -> Maybe (String, String)
        split "" = Nothing
        split (':':path) = Just ("", path)
        split (c:str) = do
            (name, path) <- split str
            return (c : name, path)

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
        (ReqArg (
            \arg opts -> case addModuleOpt arg opts of
                Right opts -> return opts
                Left err   -> ioError err
            ) "MODULE:PATH")
        "Extern module path"
    ]

getOptions :: IO Options
getOptions = do
    args <- getArgs
    let (optActions, nonOptions, errors) = getOpt Permute options args
    foldl (>>=) (return defaultOptions) optActions
module GetOpt.Internal where

import           System.Exit                    ( exitSuccess )
import           System.Console.GetOpt
import qualified Data.Text                     as T
import           System.Environment.Blank       ( getProgName )
import           Control.Monad                  ( unless
                                                , when
                                                )

data Options = Options
    { optOutput  :: FilePath
    , optModules :: [ModulePath]
    , optInputs  :: [FilePath]
    }
  deriving (Show, Eq)

data ModulePath = ModulePath
    { optModName :: T.Text
    , optModPath :: FilePath
    }
  deriving (Show, Eq)

defaultOptions :: Options
defaultOptions =
    Options { optModules = [], optOutput = "a.out", optInputs = [] }

addModuleOpt :: String -> Options -> Either IOError Options
addModuleOpt modOpt opts = do
    modPath <- splitModOpt modOpt
    return $ opts { optModules = modPath : modules }
  where
    modules = optModules opts
    splitModOpt :: String -> Either IOError ModulePath
    splitModOpt str = case split str of
        Just ("", _ )           -> Left emptyNameErr
        Just (_ , "")           -> Left emptyPathErr
        Nothing                 -> Left sepErr
        Just (modName, modPath) -> Right $ ModulePath (T.pack modName) modPath
      where
        sepErr       = userError $ '\'' : str ++ "': missing the ':' separator!"
        emptyPathErr = userError $ '\'' : str ++ "': module path is empty!"
        emptyNameErr = userError $ '\'' : str ++ "': module name is empty!"
        split :: String -> Maybe (String, String)
        split ""           = Nothing
        split (':' : path) = Just ("", path)
        split (c   : str ) = do
            (name, path) <- split str
            return (c : name, path)

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "o"
             ["output"]
             (ReqArg (\arg opts -> return opts { optOutput = arg }) "FILE")
             "Output file"
    , Option
        "V"
        ["version"]
        (NoArg
            (\_ -> do
                putStrLn "Version 0.1.0.0" -- TODO: embed the version from *.cabal
                exitSuccess
            )
        )
        "Print program version"
    , Option
        "h"
        ["help"]
        (NoArg
            (\_ -> do
                progName <- getProgName
                putStrLn (usageInfo progName options)
                exitSuccess
            )
        )
        "Show help"
    , Option
        "m"
        ["module"]
        (ReqArg
            (\arg opts -> case addModuleOpt arg opts of
                Right opts -> return opts
                Left  err  -> ioError err
            )
            "MODULE:PATH"
        )
        "Extern module path"
    ]

parseOptions :: IO [String] -> IO Options
parseOptions getArgs = do
    args <- getArgs
    let (optActions, nonOptions, errors) = getOpt Permute options args
    opts <- foldl (>>=) (return defaultOptions) optActions
    when (null nonOptions) $ ioError (userError "No input files specified!")
    unless (null errors) $ ioError (userError $ concat errors)
    return $ opts { optInputs = nonOptions }

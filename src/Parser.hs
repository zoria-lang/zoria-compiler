{-# LANGUAGE OverloadedStrings #-}
module Parser (module Parser.ParserIO, parseProgram) where

import Parser.ParserIO
import Parser.Program (program)
import Syntax (Program)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty)
import GetOpt (Options(..), ModulePath(..), getOptions)
import System.FilePath.Posix (takeDirectory)

-- IO computation which parses the program starting with the file
-- specified as command line argument.
parseProgram :: IO (Program ())
parseProgram = do
    opts   <- addRootModule <$> getOptions
    result <- runParser program "" "" newParserState opts
    case result of
        Left err -> do 
            putStrLn $ errorBundlePretty err
            exitFailure
        Right program -> return program
  where
    -- Adds the main file directory to the list of external modules specified
    -- in command line arguments. The module root name is "\".
    addRootModule :: Options -> Options
    addRootModule opts@Options {optModules = mods, optInputs = inputs} = 
        opts { optModules = rootModule : mods }
      where
        rootModule = ModulePath "\\" (takeDirectory $ head inputs)



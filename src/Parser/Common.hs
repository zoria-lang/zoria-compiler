{-# LANGUAGE OverloadedStrings #-}
module Parser.Common where

import Parser.ParserIO
import GetOpt (Options(..))
import Utility (Position(..))
import Control.Monad (void, guard, when)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.State (get, put)
import Data.Maybe (isJust)
import System.FilePath.Posix ((</>), (<.>))
import Syntax
import Text.Megaparsec ((<?>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-- TODO: decide what the path should be
-- Path to the folder with the standard library
stdLibraryDir :: FilePath
stdLibraryDir = undefined

-- File extension for Zoria files
fileExtension :: String
fileExtension = "zo"

-- Name of the "Prelude" module of the standard library
preludeName :: FilePath
preludeName = "Core" 

-- Path to the "Prelude" module of the standard library
stdPreludePath :: FilePath
stdPreludePath = stdLibraryDir </> preludeName <.> fileExtension

-- Parser combinator that lets you easily extract the current file position.
-- The argument is a function which takes a position and as a result returns
-- some parser. The result is a parser instrumented with position checking.
withPos :: P.MonadParsec e s m => (Position -> m a) -> m a
withPos f = do
    state  <- P.getParserState
    offset <- P.getOffset
    let file = P.sourceName . P.pstateSourcePos . P.statePosState $ state
    f $ Position offset file 

-- Parser combinator which makes the parser also consume the trailing whitespace
lexeme :: ParserIO a -> ParserIO a
lexeme = L.lexeme skipWhitespace

-- Function which creates a parser which consumes the given string literal
-- and the trailing space.
symbol :: T.Text -> ParserIO T.Text
symbol = L.symbol skipWhitespace

-- Parser combinator that creates a parser, which returns True if the given
-- parser matches the input (and then consumes it), or False otherwise
-- (then the output remains unconsumed)
check :: ParserIO a -> ParserIO Bool
check p = P.hidden (isJust <$> P.optional (P.try $ void p))

-- Parser which fails if the result of given parser does not match the 
-- predicate.
cond :: (a -> Bool) -> ParserIO a -> ParserIO a
cond predicate parser = do
    result <- parser
    guard (predicate result)
    return result

-- Report failure at given location instead of the current parser position.
posFail :: Position -> String -> ParserIO ()
posFail pos msg = do
    P.setOffset $ posOffset pos
    fail msg

-- Computation which extracts the command line options from the reader monad.
getopt :: ParserIO Options
getopt = liftReader ask

-- Extract the current state of the parser
getState :: ParserIO ParserState
getState = liftState get

-- Update the parser's state with the given one
putState :: ParserState -> ParserIO ()
putState state =  liftState $ put state

-- Prints a message with current parser position.
debug :: String -> ParserIO ()
debug str = withPos $ \(Position pos file) -> 
    liftIO . putStrLn $ show file ++ " " ++ show pos ++ ": " ++ str

-- Failure without an explicit cause
silentFail :: ParserIO ()
silentFail = void $ P.satisfy (const False)

-- Parser for whitespace which is supposed to be ignored.
skipWhitespace :: ParserIO ()
skipWhitespace = L.space (void P.spaceChar) lineComment blockComment
  where
    blockComment = L.skipBlockCommentNested "#:" ":#"
    -- Custom line comment parser. The L.skipLineComment wouldn't work
    -- because block and line comments share a common prefix.
    lineComment  = P.try (P.char '#' *> P.notFollowedBy ":") >> skipLine
    -- Parser which consumes all characters up to the end of a line.
    skipLine :: ParserIO ()
    skipLine = void $ P.takeWhileP (Just "character") (/= '\n')

-- Get the path of the currently parsed file.
getCurrentPath :: ParserIO FilePath
getCurrentPath = snd . head . stateModuleStack <$> getState

-- Perform some parsing computation inside a stack frame. On the stack we store
-- module names so that parser knows which module is parsed at the moment
-- and we can detect cycles.
withStack :: ModName -> FilePath -> ParserIO a -> ParserIO a
withStack name path parser = do
    pushModulePath name path
    result <- parser
    popModulePath
    return result

-- Add the currently parsed module to the stack in the parser state.
pushModulePath :: ModName -> FilePath -> ParserIO ()
pushModulePath name path = do
    state <- getState
    putState $ state { stateModuleStack = (name, path) : stateModuleStack state}

-- Remove the most recent entry on the parser state stack.
popModulePath :: ParserIO ()
popModulePath = do
    state <- getState
    putState $ state { stateModuleStack = tail . stateModuleStack $ state }

-- Parser combinator that transforms the given parser into a parser that also
-- stores the parsed thing's position.
located :: ParserIO a -> ParserIO (Located a)
located parser = withPos $ \pos -> Located pos <$> parser

-- Parser for lists starting with "start", ending with "end", separated by
-- "sep" and with elements that match the given parser "elem".
separatedList :: T.Text -> T.Text -> ParserIO a -> T.Text -> ParserIO [a]
separatedList start end elem sep = P.between start' end' (P.sepBy elem sep')
  where
    start' = symbol start
    end'   = symbol end
    sep'   = symbol sep

-- Parser combinator that turns a parser of something into a parser of the
-- same thing but between 'left' and 'right' separators (e.g parenthesis).
surroundedBy :: T.Text -> ParserIO a -> T.Text -> ParserIO a
surroundedBy left parser right = symbol left *> parser <* symbol right

-- Remove the file-specific parts of the parser state
clearLocalParserState :: ParserIO ()
clearLocalParserState = do
    state <- getState
    putState $ state { stateCurrentOps   = Map.empty
                     , stateLocalOps     = [] 
                     , stateLocalTypeOps = Map.empty
                     }

module Parser.ParserIO where

import Syntax
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import GetOpt (Options(..), ModulePath(..))
import Control.Monad.State (StateT, runStateT)
import Data.Void (Void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.IO.Class as IO
import qualified Text.Megaparsec as P
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- The used monad stack looks like this:
--   ParsecT ⊃ StateT ⊃ ReaderT ⊃ IO
-- 
-- * IO is needed to open additional files during parsing.
-- * Reader is useful to access the command line arguments.
-- * State is required so that we can easily manipulate operator tables.
-- * Parsec is used for parsing (duh)

-- Monad allowing to read command line arguments and perform IO actions.
type GetOptIO = ReaderT Options IO
-- GetOptIO monad enriched with the stateful computations. The state is used
-- to store operator tables and parsed module lists.
type StateIO  = StateT ParserState GetOptIO
-- StateIO monad enriched with the parsing abilities.
type ParserIO = P.ParsecT Void T.Text StateIO

-- Shortuct for parsing errors. We don't use the custom error component.
type Errors   = P.ParseErrorBundle T.Text Void

-- State used by StateT in the parser.
data ParserState = ParserState
    { stateCurrentOps   :: !LocalOperators
    -- ^ operators visible in the currently parsed file
    , stateLocalTypeOps :: !(Map.Map TypeName [CustomOperator])
    -- ^ operator constructors visible in the currently parser dile
    , stateLocalOps     :: ![CustomOperator]
    -- ^ list of operators defined within the current module
    , stateExportedOps  :: !GlobalOperators
    -- ^ operators that are exported by some files
    , stateVisited      :: !(Map.Map FilePath (Module ()))
    -- ^ modules that were already parsed which allows us to parse them once
    , stateModuleStack  :: ![(ModName, FilePath)]
    -- ^ stack used to keep track of current file path and to detect cycles
    }
  deriving Show

type LocalOperators  = Map.Map (Priority, Fixity) [CustomOperator]
type GlobalOperators = Map.Map FilePath [(CustomOperator, Priority, Fixity)]

-- Custom operator priority. Should be be in range 1..10.
type Priority = Int

data CustomOperator
    = ConstrOperator T.Text
    | PrefixConstrOperator T.Text
    | VarOperator T.Text
    | PrefixVarOperator T.Text
  deriving (Show, Ord, Eq)

-- Fixity of a custom operator.
data Fixity
    = LeftFix
    | RightFix
    | NoneFix
  deriving (Show, Ord, Eq)

-- Single import statement before being actually imported.
type RawImport = (ModuleId, Maybe ModName, Maybe [Located ImportedValue])
--                 ^              ^                   ^
--             imported module  alias         list of imported identifiers

-- Fresh state for the parser's internal StateT monad
newParserState :: ParserState
newParserState = ParserState
    { stateCurrentOps   = Map.empty
    , stateLocalTypeOps = Map.empty
    , stateLocalOps     = []
    , stateExportedOps  = Map.empty
    , stateVisited      = Map.empty
    , stateModuleStack  = []
    }

-- Function used to simplyfy running the parser monad stack.
-- It takes the parser to be run, file name (used for errors), input string,
-- initial StateT state and command line arguments for ReaderT monad.
-- The result is an IO computation which will either parse something or
-- fail with some errors.
runParser :: ParserIO a 
          -> FilePath
          -> T.Text 
          -> ParserState 
          -> Options
          -> IO (Either Errors a)
runParser parser file input initState options = do
    (result, _) <- runReaderT reader options
    return result
  where
    state = P.runParserT parser file input
    reader = runStateT state initState

-- Helper function used to test parsers.
testParser :: Show a => ParserIO a -> T.Text -> IO ()
testParser parser input = do
    result <- parsingResult
    case result of
        Left err     -> putStrLn $ P.errorBundlePretty err
        Right result -> putStrLn $ show result
  where
    emptyOpts = Options "" [] []
    parsingResult = runParser parser "" input newParserState emptyOpts

-- Lift the inner IO monad to our ParserIO monad
liftIO :: IO a -> ParserIO a
liftIO = IO.liftIO

-- Lift the inner State monad to our ParserIO monad
liftState :: StateIO a -> ParserIO a
liftState = lift

-- Lift the inner Reader monad to our ParserIO monad
liftReader :: GetOptIO a -> ParserIO a
liftReader = lift . lift
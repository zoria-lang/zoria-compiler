module Test.Parser.Helpers
    ( makePrettyError
    , runUntilEof
    )
where

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import           Data.Text                      ( Text )
import           Parser
import           Control.Monad                  ( replicateM_ )

untilEof :: Parser a -> Parser a
untilEof = (<* P.eof)

runUntilEof :: Parser a -> Text -> Either String a
runUntilEof p = runParser (untilEof p) ""

failAtPosition :: String -> (Int, Int) -> Parser a
failAtPosition message (lines, columns) = do
    replicateM_ (lines - 1)   skipLine
    replicateM_ (columns - 1) P.anySingle
    fail message
    where skipLine = P.many (P.anySingleBut '\n') >> P.newline

makePrettyError :: FilePath -> Text -> String -> (Int, Int) -> Either String a
makePrettyError file input msg pos =
    runParser (failAtPosition msg pos) file input

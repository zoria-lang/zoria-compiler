module Parser
    ( runParser
    )
where

import           Parser.Common
import qualified Text.Megaparsec               as P
import qualified Data.Text                     as T
import           Control.Arrow                  ( left )

runParser :: Parser a -> FilePath -> T.Text -> Either String a
runParser p f s = left P.errorBundlePretty $ P.runParser p f s

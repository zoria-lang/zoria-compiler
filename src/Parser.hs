module Parser 
    () 
where

import Lexer
import qualified Data.Text as T
import Text.Megaparsec (Parsec, ParsecT, )
import Data.Void (Void)


type IOParser = ParsecT Void T.Text IO

type Parser = Parsec Void T.Text



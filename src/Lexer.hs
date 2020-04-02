module Lexer
    ()
where 

import Text.Megaparsec (SourcePos)
import qualified Data.Text as T


data Token = Token
    { tokenPos  :: SourcePos
    , tokenType :: TokenType 
    }
  deriving (Eq, Ord, Show)

data TokenType
    = LParen
    | RParen
    | LSquareParen
    | RSquareParen
    | LArrayParen
    | RArrayParen
    | LCurlyBracket
    | RCurlyBracket
    | Backquote
    | Comma
    | Semicolon
    | Keyword KeywordType
    | Op OperatorType
    | CustomOp T.Text
    | CustomConsOp T.Text
    | UppercaseName T.Text
    | LowercaseName T.Text
    | StringTok T.Text
    | CharTok Char
    | FloatTok Double
    | IntTok Int
    | UnitLit
  deriving (Eq, Ord, Show)

data KeywordType
    = KeywordModule
    | KeywordImport
    | KeywordClass
    | KeywordInstance
    | KeywordLet
    | KeywordIn
    | KeywordWith
    | KeywordMatch
    | KeywordCase
    | KeywordAs
    | KeywordAnd
    | KeywordOr
    | KeywordFn
    | KeywordType
    | KeywordAlias
    | KeywordEnd
    | KeywordOperator
    | KeywordIf
    | KeywordThen
    | KeywordElse
    | KeywordExtern
    | KeywordInternal
    | KeywordTrue
    | KeywordFalse
  deriving (Eq, Ord, Show)

data OperatorType
    = OperatorSig
    | OperatorArrow
    | OperatorConstraint
    | OperatorPatternName
    | OperatorAssign
    | OperatorScope
  deriving (Eq, Ord, Show)

module Syntax where

import qualified Data.Text as T
import Text.Megaparsec (SourcePos)


type Name = T.Text

data Located a = Located
    { location  :: SourcePos
    , unlocated :: a
    }

newtype Program = Program 
    { getModules :: [Module] 
    }

data ModuleName = ModuleName
    { modulePrefix :: [Name]
    , moduleName   :: Name
    }

data Module = Module 
    { moduleId      :: ModuleName
    , modulePath    :: FilePath
    , moduleImports :: [Import]
    , moduleExports :: Maybe [Located Name]
    , moduleDefs    :: [TopLevelDef]
    }

data Import = Import
    { source    :: Module
    , importLoc :: SourcePos
    , importIds :: Maybe [Located Name]
    }

data TopLevelDef
    = TopLevelLet LetDef
    | AliasDef    TypeAlias
    | TypeDef     TypeDecl
    | ClassDef    Class
    | InstanceDef Instance

data LetDef = LetDef
    { letPattern :: LetPattern
    , letTypeSig :: Maybe TypeSig
    , letExpr    :: Expr
    , letLoc     :: SourcePos
    }

data Expr 
    = IntLit Int SourcePos
    -- ^ 64 bit signed integers 
    | CharLit Char SourcePos
    -- ^ single unicode character
    | FloatLit Double SourcePos
    -- ^ double precision floating number
    | StringLit T.Text SourcePos
    -- ^ non-format string literal
    | BoolLit Bool SourcePos
    -- ^ boolean value
    | UnitLit SourcePos
    -- ^ () value
    | Var Name SourcePos
    -- ^ an identifier (note th)
    | And Expr Expr SourcePos
    -- ^ 'and' boolean operator with its left and right operands
    | Or Expr Expr SourcePos
    -- ^ 'or' boolean operator with its left and right operands
    | If Expr Expr Expr SourcePos
    -- ^ conditional expression with the condition, consequence and alternative
    | Block [Expr] SourcePos
    -- ^ list of expressions to be evaluated in order
    | LetIn LetDef Expr
    -- ^ local definition. Binds only in the Expr following it. Does not
    --   store SourcePos as it is stored both in LetDef and (maybe) in Expr
    | MultiLet [LetDef] Expr
    -- ^ multiple mutually recursive local definitions.
    | Lambda [Pattern] Expr (Maybe Name) SourcePos
    -- ^ lambda expression with the list of bindings (patterns) and
    --   the expression to evaluate upon the function call.
    --   May remember the name if it was defined in 'let'-definiiton.
    | Tuple [Expr] SourcePos
    -- ^ non-empty list of expressions. Tuples of different arity
    --   have different types.
    | Array [Expr] SourcePos
    -- ^ array literal.
    | App Expr [Expr]
    -- ^ application of an expression to some expresions.
    | Match Expr [MatchCase] SourcePos
    -- ^ pattern matching of Expr with a list of patterns. First matching
    --   pattern is choosen.
    | Extern Name Name SourcePos
    -- ^ used to import foreign functions from shared libraries.
    | Internal Name SourcePos
    -- ^ built-in compiler value.
    | AnnotatedExpr Expr TypeSig SourcePos
    -- ^ expression with the type given explicitly (like 2 :: Int in Haskell).
    | FormatString [FormatExpr] SourcePos
    -- ^ interpolated string

instance Functor Located where
    fmap f (Located loc a) = Located loc $ f a
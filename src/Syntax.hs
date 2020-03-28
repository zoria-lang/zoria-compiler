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
    { letPattern :: Pattern
    , letTypeSig :: Maybe TypeSig
    , letExpr    :: Expr
    , letLoc     :: SourcePos
    }

data Expr 
    = IntLit Int SourcePos
    | CharLit Char SourcePos
    | FloatLit Double SourcePos
    | StringLit T.Text SourcePos
    | BoolLit Bool SourcePos
    | UnitLit SourcePos
    | Var Name SourcePos
    | And Expr Expr SourcePos
    | Or Expr Expr SourcePos
    | If Expr Expr Expr SourcePos
    | Block [Expr] SourcePos
    | LetIn LetDef Expr
    | MultiLet [LetDef] Expr
    | Lambda Pattern Expr SourcePos
    | Tuple [Expr] SourcePos
    | List [Expr] SourcePos
    | Array [Expr] SourcePos
    | App Expr Expr
    | Match PatternMatching
    | Extern Name Name SourcePos
    | Internal Name SourcePos
    | AnnotatedExpr Expr TypeSig SourcePos

instance Functor Located where
    fmap f (Located loc a) = Located loc $ f a

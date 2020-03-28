module Program where

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
    , moduleDefs    :: [LetDef]
    }

data Import = Import
    { source      :: Module
    , importLoc   :: SourcePos
    , importNames :: Maybe [Located Name]
    }

type LetDef = Int

data Expr 
    = IntLit Int SourcePos
    | CharLit Char SourcePos
    | FloatLit Double SourcePos
    | StringLit T.Text SourcePos
    | BoolLit Bool SourcePos
    | Unit SourcePos
    | And Expr Expr SourcePos
    | Or Expr Expr SourcePos
    | If Expr Expr Expr SourcePos

instance Functor Located where
    fmap f (Located loc a) = Located loc $ f a

module Syntax where

import qualified Data.Text as T
import Utility (Position)

newtype Identifier = Identifier T.Text deriving Show

newtype TypeVar = TypeVar T.Text deriving Show

newtype ConstructorName = ConstructorName T.Text deriving Show

newtype TypeName = TypeName T.Text deriving Show

newtype ModName = ModName T.Text deriving Show

data Located a = Located
    { location  :: Position
    , unlocated :: a
    }
  deriving Show

newtype Program a = Program 
    { programRoot :: Module a
    }
  deriving Show

data ModuleId = ModuleId
    { modulePrefix :: [ModName]
    , moduleName   :: ModName
    }
  deriving Show

data Module a = Module 
    { moduleId      :: ModuleId
    , modulePath    :: FilePath
    , moduleImports :: [Import a]
    , moduleExports :: Maybe [Located ImportedValue]
    , moduleDefs    :: [TopLevelDef a]
    }
  deriving Show

data Import a = Import
    { importMod  :: Module a
    , importName :: ModuleId
    , importLoc  :: Position
    , importIds  :: Maybe [Located ImportedValue]
    }
  deriving Show

data ImportedValue
    = ImportedIdentifier Identifier 
    -- ^ imported variable (e.g. '(>>=)', 'map')
    | ImportedType TypeName [ConstructorName]
    -- ^ type and constructor import (e.g. Maybe(Nothing), Either(), Map(..))
  deriving Show

data TopLevelDef a
    = TopLevelLet (LetDef a)
    | AliasDef    TAlias
    | TypeDef     TDef
    | ClassDef    Class
    | InstanceDef (Instance a)
  deriving Show

data LetDef a = LetDef
    { letPattern :: LetPattern a
    , letTypeSig :: Maybe TypeSig
    , letExpr    :: Expr a
    , letLoc     :: Position
    }
  deriving Show

data TAlias = TAlias
    { aliasName   :: TypeName
    , aliasParams :: [TypeVar]
    , aliasKind   :: Maybe KindSig
    , aliasType   :: TypeSig
    , alisLoc     :: Position
    }
  deriving Show

data TDef = TDef
    { typeDefName   :: TypeName
    , typeDefParams :: [TypeVar]
    , typeDefKind   :: Maybe KindSig
    , typeDefCases  :: [TypeCase]
    , typeDefLoc    :: Position
    }
  deriving Show

data Class = Class
    { className        :: TypeName
    , classConstraints :: [Constraint]
    , classParam       :: TypeVar
    , classParamKind   :: Maybe KindSig
    , classMembers     :: [ValSig]
    , classLoc         :: Position
    }
  deriving Show

data ValSig = ValSig
    { valSigName :: Identifier
    , valSigType :: TypeSig
    }
  deriving Show

data Constraint = Constraint
    { constraintName  :: TypeName
    , constraintParam :: TypeVar
    }
  deriving Show

data Instance a = Instance
    { instanceClass   :: TypeName
    , instanceType    :: TypeSig
    , instanceMembers :: [LetDef a]
    , instanceLoc     :: Position
    }
  deriving Show

data LetPattern a
    = FuncPattern 
        { letFuncName :: Identifier
        , letFuncArgs :: [Pattern a]
        }
    | LetPattern (Pattern a)
  deriving Show

data TypeSig = TypeSig
    { typeSigConstraints :: [Constraint]
    , typeSig            :: Type
    }
  deriving Show

data PrimType
    = IntT
    | FloatT
    | StringT
    | CharT
    | BoolT
    | UnitT
    | CPtrT
  deriving Show

data TypeCase
    = TypeCaseRecord ConstructorName RecordType Position
    -- ^ constructor of a record
    | TypeCase ConstructorName [TypeSig] Position
    -- ^ normal constructor (e.g. 'Just a')
  deriving Show


newtype RecordType = RecordType [RecordField] deriving Show

data RecordField = RecordField
    { recordFieldName :: Identifier
    , recordFieldType :: TypeSig
    }
  deriving Show

data KindSig
    = TypeKind
    -- ^ the * kind
    | TypeConstructorKind KindSig KindSig
    -- ^ the (->) kind
  deriving Show

data Type
    = TypeVariable  TypeVar
    -- ^ polymorphic type (e.g. 'a', 'b', 't')
    | PrimitiveType PrimType
    -- ^ One of the built-in primitive types
    | FunctionType Type Type
    -- ^ (a -> b) type
    | NonPrimType TypeName
    -- ^ non-primitive types without parameters (e.g. Integer)
    | ParamType TypeName [Type]
    -- ^ concrete type with parameters (e.g. 'Maybe a', '[a]')
    | PolymorphicParamType TypeVar [Type]
    -- ^ polymorphic type with parameters (e.g. '(m a)', '(t Int)')
    | TupleType [Type]
    -- ^ tuple of types (e.g. '(Int, a, Float)')
    | ArrayType Type
    -- ^ array of elements of concrete type (e.g '[|Int|]', [| [|Float|] |])
  deriving Show

data PrimExpr
    = IntLit Int
    -- ^ 64 bit signed integers 
    | CharLit Char
    -- ^ single unicode character
    | FloatLit Double
    -- ^ double precision floating number
    | StringLit T.Text
    -- ^ non-format string literal
    | BoolLit Bool
    -- ^ boolean value
    | UnitLit
    -- ^ () value
  deriving Show

data Expr a
    = Primitive PrimExpr Position a
    | Var Identifier Position a
    -- ^ an identifier
    | ScopedName ModuleId Identifier Position a
    -- ^ name from a module (e.g. `Foo.Bar.x`)
    | And (Expr a) (Expr a) Position a
    -- ^ 'and' boolean operator with its left and right operands
    | Or (Expr a) (Expr a) Position a
    -- ^ 'or' boolean operator with its left and right operands
    | If (Expr a) (Expr a) (Expr a) Position a
    -- ^ conditional expression with the condition, consequence and alternative
    | Block [Expr a] Position a
    -- ^ list of expressions to be evaluated in order
    | LetIn (LetDef a) (Expr a) a
    -- ^ local definition. Binds only in the Expr following it. Does not
    --   store Position as it is stored both in LetDef and (maybe) in Expr
    | MultiLetIn [LetDef a] (Expr a) a
    -- ^ multiple mutually recursive local definitions.
    | Lambda [Pattern a] (Expr a) (Maybe Identifier) Position a
    -- ^ lambda expression with the list of bindings (patterns) and
    --   the expression to evaluate upon the function call.
    --   May remember the name if it was defined in 'let'-definiiton.
    | Tuple [Expr a] Position a
    -- ^ non-empty list of expressions. Tuples of different arity
    --   have different types.
    | Array [Expr a] Position a
    -- ^ array literal.
    | App (Expr a) (Expr a) a
    -- ^ application of an expression to another expression.
    | Match (Expr a) [MatchCase a] Position a
    -- ^ pattern matching of Expr with a list of patterns. First matching
    --   pattern is choosen.
    | Extern FilePath T.Text Position a
    -- ^ used to import foreign functions from shared libraries.
    | Internal Identifier Position a
    -- ^ built-in compiler value.
    | AnnotatedExpr (Expr a) TypeSig Position a
    -- ^ expression with the type given explicitly (like 2 :: Int in Haskell).
    | FormatString [FormatExpr a] Position a
    -- ^ string literals and expressions to evaluate, show and concatenate
  deriving Show

data FormatExpr a
    = FmtStr  T.Text
    | FmtExpr (Expr a)
  deriving Show

data MatchCase a = MatchCase
    { matchCasePattern :: Pattern a
    , matchCaseExpr    :: Expr a
    }
  deriving Show

data Pattern a
    = ConstPattern PrimExpr Position a
    -- ^ a primitive constant (e.g. 1)
    | TuplePattern [Pattern a] Position a
    -- ^ a tupple of patterns (e.g. (x, _))
    | ConstructorPattern ConstructorName [Pattern a] Position a
    -- ^ a constructor applied to patterns (e.g. 'Just x', 'Nothing', 'x:xs')
    | WildcardPattern Position a
    -- ^ a pattern that matches everything but discards the value ('_')
    | VarPattern Identifier Position a
    -- ^ a pattern that matches everything and binds the value to the name
    | NamedPattern Identifier (Pattern a) Position a
    -- ^ a pattern that is named as a whole (e.g. 'tree@(Node left x right)')
  deriving Show

instance Functor Located where
    fmap f (Located loc a) = Located loc $ f a
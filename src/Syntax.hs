module Syntax where

import qualified Data.Text as T
import Utility (Position)

newtype Identifier = Identifier T.Text deriving (Show, Eq, Ord)

newtype TypeVar = TypeVar T.Text deriving (Show, Eq, Ord)

newtype ConstructorName = ConstructorName T.Text deriving (Show, Eq, Ord)

newtype TypeName = TypeName T.Text deriving (Show, Eq, Ord)

newtype ModName = ModName T.Text deriving (Show, Eq, Ord)

data Located a = Located
    { location  :: Position
    , unlocated :: a
    }
  deriving (Show, Eq)

newtype Program a = Program 
    { programRoot :: Module a
    }
  deriving (Show, Eq)

data ModuleId = ModuleId
    { modulePrefix :: [ModName]
    , moduleName   :: ModName
    }
  deriving (Show, Eq, Ord)

data Module a = Module 
    { moduleId      :: ModuleId
    , modulePath    :: FilePath
    , moduleImports :: [Import a]
    , moduleExports :: Maybe [Located ImportedValue]
    , moduleDefs    :: [TopLevelDef a]
    }
  deriving (Show, Eq)

data Import a = Import
    { importMod   :: Module a
    , importName  :: ModuleId
    , importAlias :: Maybe ModName
    , importLoc   :: Position
    , importIds   :: Maybe [Located ImportedValue]
    }
  deriving (Show, Eq)

data ImportedValue
    = ImportedIdentifier Identifier 
    -- ^ imported variable (e.g. '(>>=)', 'map')
    | ImportedType TypeName (Maybe [ConstructorName])
    -- ^ type and constructor import (e.g. Maybe(Nothing), Either(), Map(..))
  deriving (Show, Eq, Ord)

data TopLevelDef a
    = TopLevelLet (LetDef a)
    | AliasDef    TAlias
    | TypeDef     TDef
    | ClassDef    Class
    | InstanceDef (Instance a)
  deriving (Show, Eq)

data Definition a = Definition
    { letPattern :: Pattern a
    , letTypeSig :: Maybe TypeSig
    , letExpr    :: Expr a
    , letLoc     :: Position
    }
  deriving (Show, Eq)

data LetDef a
    = LetDef (Definition a)
    -- ^ single non-recursive definition 
    | LetRecDef [Definition a] Position
    -- ^ a set of mutually recursive definitions.
    --   contains the position of 'let-rec' keyword
  deriving (Eq, Show)

data TAlias = TAlias
    { aliasName   :: TypeName
    , aliasParams :: [TypeVar]
    , aliasKind   :: Maybe KindSig
    , aliasType   :: Type
    , aliasLoc    :: Position
    }
  deriving (Show, Eq)

data TDef = TDef
    { typeDefName   :: TypeName
    , typeDefParams :: [TypeVar]
    , typeDefKind   :: Maybe KindSig
    , typeDefCases  :: [TypeCase]
    , typeDefLoc    :: Position
    }
  deriving (Show, Eq)

data Class = Class
    { className        :: TypeName
    , classConstraints :: [Constraint]
    , classParam       :: TypeVar
    , classParamKind   :: Maybe KindSig
    , classMembers     :: [ValSig]
    , classLoc         :: Position
    }
  deriving (Show, Eq)

data ValSig = ValSig
    { valSigName :: Identifier
    , valSigType :: TypeSig
    }
  deriving (Show, Eq)

data Constraint = Constraint
    { constraintName  :: TypeName
    , constraintParam :: TypeVar
    }
  deriving (Show, Eq, Ord)

data Instance a = Instance
    { instanceClass       :: TypeName
    , instanceType        :: TypeSig
    , instanceMembers     :: [Definition a]
    , instanceLoc         :: Position
    , instanceConstraints :: [Constraint]
    }
  deriving (Show, Eq)

data TypeSig = TypeSig
    { typeSigConstraints :: [Constraint]
    , typeSig            :: Type
    }
  deriving (Show, Eq, Ord)

data PrimType
    = IntT
    | FloatT
    | StringT
    | CharT
    | BoolT
    | UnitT
    | CPtrT
  deriving (Show, Eq, Ord)

data TypeCase
    = TypeCaseRecord ConstructorName RecordType Position
    -- ^ constructor of a record
    | TypeCase ConstructorName [Type] Position
    -- ^ normal constructor (e.g. 'Just a')
  deriving (Show, Eq, Ord)


newtype RecordType = RecordType [RecordField] deriving (Show, Eq, Ord)

data RecordField = RecordField
    { recordFieldName :: Identifier
    , recordFieldType :: Type
    }
  deriving (Show, Eq, Ord)

data KindSig
    = TypeKind
    -- ^ the * kind
    | TypeConstructorKind KindSig KindSig
    -- ^ the (->) kind
  deriving (Eq, Show)

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
    -- ^ array of elements of some type (e.g '[>Int<]', [>[>a<]<])
  deriving (Show, Eq, Ord)

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
  deriving (Eq, Show)

data Expr a
    = Primitive PrimExpr Position a
    | Var Identifier Position a
    -- ^ an identifier (e.g. map, (++))
    | Constructor ConstructorName Position a
    -- ^ a constructor (e.g. Just, [], ::, Nothing)
    | QualifiedVar ModName Identifier Position a
    -- ^ an explicitly qualified identifier (e.g. Foo\bar)
    | QualifiedConstructor ModName ConstructorName Position a
    -- ^ an explicitly qualified constructor (e.g. Foo\Nothing)
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
    --   store Position as it is stored both in Definition and (maybe) in Expr
    | Lambda (Pattern a) (Expr a) (Maybe T.Text) Position a
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
    | External FilePath T.Text Position a
    -- ^ used to import foreign functions from shared libraries.
    | Internal Identifier Position a
    -- ^ built-in compiler value.
    | AnnotatedExpr (Expr a) TypeSig Position a
    -- ^ expression with the type given explicitly (like 2 :: Int in Haskell).
    | FormatString [FormatExpr a] Position a
    -- ^ string literals and expressions to evaluate, show and concatenate
  deriving (Eq, Show)

data FormatExpr a
    = FmtStr  T.Text
    | FmtExpr (Expr a)
  deriving (Eq, Show)

data MatchCase a = MatchCase
    { matchCasePattern :: Pattern a
    , matchCaseExpr    :: Expr a
    }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

instance Functor Located where
    fmap f (Located loc a) = Located loc $ f a
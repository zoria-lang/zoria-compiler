module Syntax where

import qualified Data.Text as T
import Text.Megaparsec (SourcePos)


newtype Identifier = Identifier T.Text

newtype TypeVar = TypeVar T.Text

newtype TypeName = TypeName T.Text

newtype ModName = ModName T.Text

type Name = T.Text

data Located a = Located
    { location  :: SourcePos
    , unlocated :: a
    }

newtype Program = Program 
    { getModules :: [Module] 
    }

data ModuleId = ModuleId
    { modulePrefix :: [ModName]
    , moduleName   :: ModName
    }

data Module = Module 
    { moduleId      :: ModuleId
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
    | AliasDef    TAlias
    | TypeDef     TDef
    | ClassDef    Class
    | InstanceDef Instance

data LetDef = LetDef
    { letPattern :: LetPattern
    , letTypeSig :: Maybe TypeSig
    , letExpr    :: Expr
    , letLoc     :: SourcePos
    }

data TAlias = TAlias
    { aliasName   :: TypeName
    , aliasParams :: [TypeVar]
    , aliasKind   :: Maybe KindSig
    , aliasType   :: TypeSig
    , alisLoc     :: SourcePos
    }

data TDef = TDef
    { typeDefName   :: TypeName
    , typeDefParams :: [TypeVar]
    , typeDefKind   :: Maybe KindSig
    , typeDefCases  :: [TypeCase]
    , typeDefLoc    :: SourcePos
    }

data Class = Class
    { className        :: TypeName
    , classConstraints :: [Constraint]
    , classParam       :: TypeVar
    , classParamKind   :: Maybe KindSig
    , classMembers     :: [ValSig]
    , classLoc         :: SourcePos
    }

data ValSig = ValSig
    { valSigName :: Name
    , valSigType :: TypeSig
    }

data Constraint = Constraint
    { constraintName  :: TypeName
    , constraintParam :: TypeVar
    }

data Instance = Instance
    { instanceClass   :: TypeName
    , instanceType    :: TypeSig
    , instanceMembers :: [LetDef]
    , instanceLoc     :: SourcePos
    }

data LetPattern
    = FuncPattern 
        { letFuncName  :: Name
        , letFuncArgs :: [Pattern]
        }
    | LetPattern Pattern

data TypeSig = TypeSig
    { typeSigConstraints :: [Constraint]
    , typeSig            :: Type
    }

data PrimType
    = IntT
    | FloatT
    | StringT
    | CharT
    | BoolT
    | UnitT

data TypeCase
    = TypeCaseRecord TypeName RecordType SourcePos
    -- ^ constructor of a record
    | TypeCase TypeName [TypeSig] SourcePos
    -- ^ normal constructor (eg. 'Just a')

newtype RecordType = RecordType [RecordField]

data RecordField = RecordField
    { recordFieldName :: Name
    , recordFieldType :: TypeSig
    }

data KindSig
    = TypeKind
    -- ^ the * kind
    | TypeConstructorKind
    -- ^ the (->) kind

data Type
    = TypeVariable  TypeVar
    -- ^ polymorphic type (eg. 'a', 'b', 't')
    | PrimitiveType PrimType
    -- ^ One of the built-in primitive types
    | FunctionType Type Type
    -- ^ (a -> b) type
    | NonPrimType TypeName
    -- ^ non-primitive types without parameters (eg. Integer)
    | ParamType TypeName [Type]
    -- ^ concrete type with parameters (eg. 'Maybe a', '[a]', 'Array Int')
    | PolymorphicParamType TypeVar [Type]
    -- ^ polymorphic type with parameters (eg. '(m a)', '(t Int)')
    | TupleType [Type]
    -- ^ tuple of types (eg. '(Int, a, Float)')

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
    -- ^ an identifier
    | Constructor Name SourcePos
    -- ^ an identifier which is a type constructor
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
    | MultiLetIn [LetDef] Expr
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
    | App Expr Expr
    -- ^ application of an expression to another expression.
    | Match Expr [MatchCase] SourcePos
    -- ^ pattern matching of Expr with a list of patterns. First matching
    --   pattern is choosen.
    | Extern FilePath T.Text SourcePos
    -- ^ used to import foreign functions from shared libraries.
    | Internal Name SourcePos
    -- ^ built-in compiler value.
    | AnnotatedExpr Expr TypeSig SourcePos
    -- ^ expression with the type given explicitly (like 2 :: Int in Haskell).
    | FormatString [FormatExpr] SourcePos
    -- ^ string literals and expressions to evaluate, show and concatenate

data FormatExpr
    = FmtStr  T.Text
    | FmtExpr Expr

instance Functor Located where
    fmap f (Located loc a) = Located loc $ f a
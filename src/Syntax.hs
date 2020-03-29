module Syntax where

import qualified Data.Text as T
import Text.Megaparsec (SourcePos)


newtype Identifier = Identifier T.Text

newtype TypeVar = TypeVar T.Text

newtype ConstructorName = ConstructorName T.Text

newtype TypeName = TypeName T.Text

newtype ModName = ModName T.Text

data Located a = Located
    { location  :: SourcePos
    , unlocated :: a
    }

newtype Program = Program 
    { programModules :: [Module] 
    }

data ModuleId = ModuleId
    { modulePrefix :: [ModName]
    , moduleName   :: ModName
    }

data Module = Module 
    { moduleId      :: ModuleId
    , modulePath    :: FilePath
    , moduleImports :: [Import]
    , moduleExports :: Maybe [Located Identifier]
    , moduleDefs    :: [TopLevelDef]
    }

data Import = Import
    { source    :: Module
    , importLoc :: SourcePos
    , importIds :: Maybe [Located Identifier]
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
    { valSigName :: Identifier
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
        { letFuncName :: Identifier
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
    | CPtrT

data TypeCase
    = TypeCaseRecord ConstructorName RecordType SourcePos
    -- ^ constructor of a record
    | TypeCase ConstructorName [TypeSig] SourcePos
    -- ^ normal constructor (e.g. 'Just a')

newtype RecordType = RecordType [RecordField]

data RecordField = RecordField
    { recordFieldName :: Identifier
    , recordFieldType :: TypeSig
    }

data KindSig
    = TypeKind
    -- ^ the * kind
    | TypeConstructorKind KindSig KindSig
    -- ^ the (->) kind

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
    -- ^ concrete type with parameters (e.g. 'Maybe a', '[a]', 'Array Int')
    | PolymorphicParamType TypeVar [Type]
    -- ^ polymorphic type with parameters (e.g. '(m a)', '(t Int)')
    | TupleType [Type]
    -- ^ tuple of types (e.g. '(Int, a, Float)')

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

data Expr 
    = Primitive PrimExpr SourcePos
    | Var Identifier SourcePos
    -- ^ an identifier
    | ScopedName ModuleId Identifier SourcePos
    -- ^ name from a module (e.g. `Foo.Bar.x`)
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
    | Lambda [Pattern] Expr (Maybe Identifier) SourcePos
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
    | Internal Identifier SourcePos
    -- ^ built-in compiler value.
    | AnnotatedExpr Expr TypeSig SourcePos
    -- ^ expression with the type given explicitly (like 2 :: Int in Haskell).
    | FormatString [FormatExpr] SourcePos
    -- ^ string literals and expressions to evaluate, show and concatenate

data FormatExpr
    = FmtStr  T.Text
    | FmtExpr Expr

data MatchCase = MatchCase
    { matchCasePattern :: Pattern
    , matchCaseExpr    :: Expr
    }

data Pattern
    = ConstPattern PrimExpr SourcePos
    -- ^ a primitive constant (e.g. 1)
    | TuplePattern [Pattern] SourcePos
    -- ^ a tupple of patterns (e.g. (x, _))
    | ConstructorPattern ConstructorName [Pattern] SourcePos
    -- ^ a constructor applied to patterns (e.g. 'Just x', 'Nothing', 'x:xs')
    | WildcardPattern SourcePos
    -- ^ a pattern that matches everything but discards the value ('_')
    | VarPattern Identifier SourcePos
    -- ^ a pattern that matches everything and binds the value to the name
    | NamedPattern Identifier Pattern SourcePos
    -- ^ a pattern that is named as a whole (e.g. 'tree@(Node left x right)')

instance Functor Located where
    fmap f (Located loc a) = Located loc $ f a
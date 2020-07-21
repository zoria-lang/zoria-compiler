module Parser.RawAst where

import Utility (Position)
import qualified Data.Text                     as T
import qualified Syntax                        as Ast

-- Types defined in this module mirror the ones from the Syntax module.
-- They exist as an intermediate representation for parsed programs. The
-- parser first parses separate files into Module type and then all of
-- them are joined into a single tree. This is done like this because
-- we parse custom operators in two stages.
-- TODO: Parametrize the Module type from Syntax by some type which
--       represents expressions (raw / parsed)

type Program = [Module]

data Module = Module
    { modPath :: FilePath
    , modData :: RawModule
    }
  deriving (Eq, Show)
 
data RawModule = RawModule
    { modId      :: Ast.ModuleId
    , modExports :: IdentifierList
    , modImports :: [Ast.Located Import]
    , modDefs    :: [TopLevelDef]
    }
  deriving (Eq, Show)

data IdentifierList
    = Everything
    | Specified [Ast.Located Ast.ImportedValue]
  deriving (Eq, Show)

data Import = Import
    { importFrom  :: Ast.ModuleId
    , importAlias :: Maybe Ast.ModName
    , importIds   :: IdentifierList
    }
  deriving (Eq, Show)

data TopLevelDef
    = OperatorDef OpDecl
    | GlobalLet   LetDef
    | AliasDef    Ast.TAlias
    | TypeDef     Ast.TDef
    | ClassDef    Ast.Class
    | InstanceDef Instance
  deriving (Eq, Show)

data Instance = Instance
    { instanceClass       :: Ast.TypeName
    , instanceType        :: Ast.TypeSig
    , instanceMembers     :: [Definition]
    , instanceConstraints :: [Ast.Constraint]
    , instanceLoc         :: Position
    }
  deriving (Eq, Show)

data OpDecl = OpDecl
    { opName   :: Ast.Identifier
    , opFixity :: OpFixity
    , opPrec   :: Int
    }
  deriving (Eq, Show)

data OpFixity 
    = LeftFix 
    | NoneFix 
    | RightFix 
  deriving  (Eq, Show)

data CustomOp
    = NormalOp T.Text
    | BacktickOp T.Text
  deriving (Eq, Show)

data LetDef 
    = LetDef    Definition
    | LetRecDef [Definition] Position
  deriving (Eq, Show)

data Definition = Definition
    { letPattern :: Pattern
    , letTypeSig :: Maybe Ast.TypeSig    
    , letExpr    :: Expr
    , letLoc     :: Position
    }
  deriving (Eq, Show)

data Pattern 
    = ConstPattern Ast.PrimExpr Position 
    | TuplePattern [Pattern] Position
    | WildcardPattern Position
    | VarPattern Ast.Identifier Position
    | NamedPattern Ast.Identifier Pattern Position
    | PrefixAppPattern [Pattern] Position
    | UnparsedPattern UnparsedPattern
  deriving (Eq, Show)

data UnparsedPattern
    = SinglePattern Pattern
    | BinOpPattern  BinOpPat
  deriving (Eq, Show)

data BinOpPat = BinOpPat
    { opPatternLhs :: Pattern
    , opPatternOp  :: CustomOp
    , opPatternRhs :: UnparsedPattern
    , opPatternPos :: Position
    }
  deriving (Eq, Show)

data Expr
    = Primitive Ast.PrimExpr Position
    | Var Ast.Identifier Position
    | Constructor Ast.ConstructorName
    | Qualifiedvar Ast.ModName Ast.Identifier Position
    | And Expr Expr Position
    | Or Expr Expr Position
    | If Expr Expr Expr Position
    | Block [Expr] Position
    | LetIn LetDef Expr
    | Lambda Pattern Expr (Maybe Ast.Identifier) Position
    | Tuple [Expr] Position
    | Array [Expr] Position
    | Match Expr [MatchCase] Position
    | External FilePath T.Text Position
    | Internal Ast.Identifier Position
    | AnnotatedExpr Expr Ast.TypeSig Position
    | FormatString [FormatExpr] Position
    | PrefixApp Expr Expr
    | UnparsedExpr UnparsedExpr
  deriving (Eq, Show)

data UnparsedExpr
    = SingleExpr Expr
    | BinOpExpr BinOpExpr'
  deriving (Eq, Show)

data BinOpExpr' = BinOpExpr'
    { opExprLhs :: Expr
    , opExprOp  :: CustomOp
    , opExprRhs :: UnparsedExpr
    , opExprPos :: Position
    }
  deriving (Eq, Show)

data MatchCase = MatchCase
    { matchCasePattern :: Pattern
    , matchCaseExpr    :: Expr
    }
  deriving (Eq, Show)

data FormatExpr
    = FmtStr T.Text
    | FmtExpr Expr
  deriving (Eq, Show)

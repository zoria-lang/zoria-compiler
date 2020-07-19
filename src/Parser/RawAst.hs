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
    { modId      :: Ast.ModuleId
    , modPath    :: FilePath
    , modExports :: IdentifierList
    , modImports :: [Ast.Located Import]
    , modDefs    :: [TopLevelDef]
    }

data IdentifierList
    = Everything
    | Specified [Ast.Located Ast.ImportedValue]

data Import = Import
    { importFrom  :: Ast.ModuleId
    , importAlias :: Maybe Ast.ModuleId
    , importIds   :: IdentifierList
    }

data TopLevelDef
    = OperatorDef OpDecl
    | GlobalLet   LetDef
    | AliasDef    Ast.TAlias
    | TypeDef     Ast.TDef
    | ClassDef    Ast.Class
    | InstanceDef Instance

data Instance = Instance
    { instanceClass       :: Ast.TypeName
    , instanceType        :: Ast.TypeSig
    , instanceMembers     :: [Definition]
    , instanceConstraints :: [Ast.Constraint]
    , instanceLoc         :: Position
    }

data OpDecl = OpDecl
    { opName   :: Ast.Identifier
    , opFixity :: OpFixity
    , opPrec   :: Int
    }

data OpFixity = LeftFix | NoneFix | RightFix

data CustomOp
    = NormalOp T.Text
    | BacktickOp T.Text

data LetDef 
    = LetDef    Definition
    | LetRecDef [Definition] Position

data Definition = Definition
    { letPattern :: Pattern
    , letTypeSig :: Maybe Ast.TypeSig    
    , letExpr    :: Expr
    , letLoc     :: Position
    }

data Pattern 
    = ConstPattern Ast.PrimExpr Position 
    | TuplePattern [Pattern] Position
    | WildcardPattern Position
    | VarPattern Ast.Identifier Position
    | NamedPattern Ast.Identifier Pattern Position
    | PrefixAppPattern [Pattern] Position
    | UnparsedPattern UnparsedPattern

data UnparsedPattern
    = SinglePattern Pattern
    | BinOpPattern  BinOpPat

data BinOpPat = BinOpPat
    { opPatternLhs :: Pattern
    , opPatternOp  :: CustomOp
    , opPatternRhs :: UnparsedPattern
    , opPatternPos :: Position
    }

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

data UnparsedExpr
    = SingleExpr Expr
    | BinOpExpr BinOpExpr'

data BinOpExpr' = BinOpExpr'
    { opExprLhs :: Expr
    , opExprOp  :: CustomOp
    , opExprRhs :: UnparsedExpr
    , opExprPos :: Position
    }

data MatchCase = MatchCase
    { matchCasePattern :: Pattern
    , matchCaseExpr    :: Expr
    }

data FormatExpr
    = FmtStr T.Text
    | FmtExpr Expr
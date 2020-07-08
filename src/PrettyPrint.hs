{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import Syntax
import qualified Data.Text as T


class PrettyPrint a where
    prettyPrint :: Int -> a -> T.Text

indent :: Int -> T.Text
indent i = T.concat (replicate i "\t")

instance PrettyPrint (Expr a) where
    prettyPrint i (Primitive p _ _) = 
        indent i <> (prettyPrint i p)
    prettyPrint i (Var (Identifier var) _ _) = 
        indent i <> var
    prettyPrint i (Constructor (ConstructorName var) _ _) = 
        indent i <> var
    prettyPrint i (QualifiedVar (ModName mod) (Identifier var) _ _) = 
        indent i <> mod <> "\\" <> var
    prettyPrint i (QualifiedConstructor (ModName mod) (ConstructorName c) _ _) =
        indent i <> mod <> "\\" <> c
    prettyPrint i (And l r _ _) = 
        indent i <> prettyPrint 0 l <> " and " <> prettyPrint 0 r
    prettyPrint i (Or l r _ _) = 
        indent i <> prettyPrint 0 l <> " or " <> prettyPrint 0 r
    prettyPrint i (If cond then' else' _ _) = 
        indent i <> ("if " <> prettyPrint  0 cond) <> "\n" 
                 <> indent (i + 1) <> "then " <> prettyPrint 0 then' <> "\n"
                 <> indent (i + 1) <> "else " <> prettyPrint 0 else' <> "\n"
    prettyPrint i (Block exprs _ _) = 
        indent i <> "{\n" <> T.concat (map aux exprs) <> indent i <> "}\n"
      where
        aux expr = prettyPrint (i + 1) expr <> ";\n"
    prettyPrint i (LetIn def expr _) = 
        prettyPrint i def <> " in\n" <> prettyPrint (i + 1) expr
    prettyPrint i (Lambda pat expr _ _ _) = 
        indent i <> "fn " 
                 <> prettyPrint 0 pat <> " => " 
                 <> prettyPrint 0 expr
    prettyPrint i (Tuple exprs _ _) = 
        indent i 
            <> "(" 
            <> T.concat (map ((<> ", ") . prettyPrint 0) exprs) 
            <> ")"
    prettyPrint i (Array exprs _ _) =
        indent i 
            <> "[<" 
            <> T.concat (map ((<> ", ") . prettyPrint 0) exprs) 
            <> ">]"
    prettyPrint i (App f arg a) =
        indent i <> "(" <> prettyPrint 0 f <> " " <> prettyPrint 0 arg <> ")"
    prettyPrint i (Match expr cases _ _) =
        indent i <> "match " <> prettyPrint 0 expr <> " with\n"
                 <> T.concat (map ((<> "\n") . prettyPrint (i + 1)) cases)
    prettyPrint i (External path id _ _) = 
        indent i <> "(_external " <> id <> " " <> id <> ")"
    prettyPrint i (Internal (Identifier id) _ _) =
        indent i <> "(_internal " <> id <> ")"
    prettyPrint i (AnnotatedExpr expr sig _ _) =
        indent i <> prettyPrint 0 expr <> " : " <> prettyPrint 0 sig
    prettyPrint i (FormatString fmts _ _) =
        indent i <> "\"" <> T.concat (map (prettyPrint 0) fmts) <> "\""

instance PrettyPrint (MatchCase a) where
    prettyPrint i (MatchCase pattern expr) =
        indent i 
            <> "case " <> prettyPrint 0 pattern 
            <> " => "  <> prettyPrint 0 expr

instance PrettyPrint (FormatExpr a) where
    prettyPrint i (FmtStr str) = indent i <> str
    prettyPrint i (FmtExpr expr) = 
        indent i <> "{" <> prettyPrint 0 expr <> "}"

instance PrettyPrint PrimExpr where
    prettyPrint i (IntLit int)  = indent i <> T.pack (show int)
    prettyPrint i (CharLit c)   = indent i <> "'" <> T.cons c "'"
    prettyPrint i (FloatLit f)  = indent i <> T.pack (show f)
    prettyPrint i (StringLit s) = indent i <> "\"" <> s <> "\""
    prettyPrint i (BoolLit b)   = indent i <> T.pack (show b)
    prettyPrint i UnitLit       = indent i <> "()"

instance PrettyPrint TypeSig where
    prettyPrint i (TypeSig constraints typeSig) =
        indent i 
        <> "(" <> T.concat (map ((<>  ", ") . prettyPrint 0) constraints) <> ")"
        <> " => " <> prettyPrint 0 typeSig

instance PrettyPrint Constraint where
    prettyPrint i (Constraint (TypeName name) (TypeVar param)) =
        indent i <> name <> " " <> param

instance PrettyPrint Type where
    prettyPrint i (TypeVariable (TypeVar var)) = indent i <> var
    prettyPrint i (PrimitiveType t) = indent i <> prettyPrint 0 t
    prettyPrint i (FunctionType from to) = 
        indent i 
            <> "(" 
            <> prettyPrint 0 from 
            <> " -> " 
            <> prettyPrint 0 to 
            <> ")"
    prettyPrint i (NonPrimType (TypeName t)) = indent i <> t
    prettyPrint i (ParamType (TypeName t) args) =
        indent i <> t <> T.concat (map ((" " <>) . prettyPrint 0) args)
    prettyPrint i (PolymorphicParamType (TypeVar var) args) =
        indent i <> var <> T.concat (map ((" " <>) . prettyPrint 0) args)
    prettyPrint i (TupleType types) =
        indent i <> T.concat (map ((<> ", ") . prettyPrint 0) types)
    prettyPrint i (ArrayType t) =
        indent i <> "[<" <> prettyPrint 0 t <> ">]"

instance PrettyPrint PrimType where
    prettyPrint i IntT    = indent i <> "Int"
    prettyPrint i FloatT  = indent i <> "Float"
    prettyPrint i StringT = indent i <> "String"
    prettyPrint i CharT   = indent i <> "Char"
    prettyPrint i BoolT   = indent i <> "Bool"
    prettyPrint i UnitT   = indent i <> "()"
    prettyPrint i CPtrT   = indent i <> "CPtr"

instance PrettyPrint (Pattern a) where
    prettyPrint i (NamedPattern (Identifier name) pattern _ _) =
        indent i <> name <> "@" <> prettyPrint 0 pattern
    prettyPrint i (VarPattern (Identifier var) _ _) = indent i <> var
    prettyPrint i (WildcardPattern _ _) = indent i <> "_"
    prettyPrint i (ConstructorPattern (ConstructorName constr) args _ _) =
        indent i 
            <> "(" 
            <> constr 
            <> T.concat (map ((<> " ") . prettyPrint 0) args)
            <> ")"
    prettyPrint i (TuplePattern patterns _ _) =
        indent i
            <> "("
            <> T.concat (map ((<> ", ") . prettyPrint 0) patterns)
            <> ")"
    prettyPrint i (ConstPattern expr _ _) =
        indent i <> prettyPrint 0 expr

instance PrettyPrint (LetDef a) where
    prettyPrint i (LetDef def) =
        indent i <> "let " <> prettyPrint 0 def
    prettyPrint i (LetRecDef defs _) =
        indent i 
        <> "let-rec {\n"
        <> T.concat (map ((<> "\n") . prettyPrint (i + 1)) defs)
        <> indent i <> "\n}"

instance PrettyPrint (Definition a) where
    prettyPrint i (Definition pattern sig expr _) =
        indent i 
            <> prettyPrint 0 pattern 
            <> " : " 
            <> maybePrint sig 
            <> " = "
            <> prettyPrint 0 expr
      where
        maybePrint Nothing = ""
        maybePrint (Just sig) = prettyPrint 0 sig

instance PrettyPrint (TopLevelDef a) where
    prettyPrint i (TopLevelLet def) = prettyPrint i def
    prettyPrint i (AliasDef alias)  = prettyPrint i alias
    prettyPrint i (TypeDef tdef)    = prettyPrint i tdef
    prettyPrint i _ = undefined

instance PrettyPrint TDef where
    prettyPrint i (TDef (TypeName name) params _ cases _) =
        indent i 
            <> "type " 
            <> name <> " " 
            <> T.concat (map (\(TypeVar var) -> var <> " ") params)
            <> " where\n"
            <> T.concat (map ((<> "\n") . prettyPrint (i + 1)) cases)

instance PrettyPrint TypeCase where
    prettyPrint i (TypeCase (ConstructorName name) params _) =
        indent i
            <> "case "
            <> name <> " "
            <> T.concat (map ((\s -> "(" <> s <> ") ") . prettyPrint 0) params)
    prettyPrint i (TypeCaseRecord (ConstructorName name) record _) =
        indent i <> "case " <> name <> " " <> prettyPrint 0 record

instance PrettyPrint RecordType where
    prettyPrint i (RecordType fields) =
        indent i
            <> "{"
            <> T.concat (map (T.cons ' ' . prettyPrint 0) fields)
            <> " } "

instance PrettyPrint RecordField where
    prettyPrint i (RecordField (Identifier name) sig) =
        indent i <> name <> " : " <> prettyPrint 0 sig

instance PrettyPrint TAlias where
    prettyPrint i (TAlias (TypeName name) params _ type' _) =
        indent i
            <> "alias " <> name <> " "
            <> T.concat (map (\(TypeVar var) -> var <> " ") params) 
            <> " := "
            <> prettyPrint 0 type'

instance PrettyPrint (Program a) where
    prettyPrint i (Program root) = prettyPrint i root

instance PrettyPrint (Module a) where
    prettyPrint i (Module id _ imports exports defs) =
        indent i <> "module " <> prettyPrint 0 id <> ":\n"
            <> T.concat (map ((<> "\n") . prettyPrint i) defs)
            <> indent i <> "imports: (\n"
            <> T.concat (map (prettyPrint (i + 1) . importMod) imports)
            <> "\n" <> indent i <> ")\n"

instance PrettyPrint ModuleId where
    prettyPrint i (ModuleId prefix (ModName name)) =
        indent i <> T.concat (map aux prefix) <> name
      where
        aux (ModName name) = name <> "\\"
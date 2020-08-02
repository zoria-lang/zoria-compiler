{-# LANGUAGE OverloadedStrings #-}
module Parser.Type where

import Parser.ParserIO
import Parser.Common
import Parser.Identifier
import Syntax
import Text.Megaparsec ((<?>))
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Text.Megaparsec as P
import qualified Data.Text as T

-- Parser for type names (e.g. Maybe, Int, [])
typeName :: ParserIO T.Text
typeName = (uppercaseName <|> symbol "[]") <?> "type name"

-- Parser for polymorphic type variables
typeVariable :: ParserIO T.Text
typeVariable = P.try (lowercaseName <?> "type variable")

-- Parser for explicit type signatures.
typeSignature :: ParserIO TypeSig
typeSignature = do
    context <- P.optional . P.try $ constraints <* symbol "=>"
    sig     <- type'
    return $ TypeSig (concat context) sig

-- Parser for constraints in type signatures.
constraints :: ParserIO [Constraint]
constraints = pure <$> singleConstraint
          <|> separatedList "(" ")" singleConstraint ","
  where
    singleConstraint :: ParserIO Constraint
    singleConstraint = do
        className  <- TypeName <$> typeName
        classParam <- TypeVar  <$> typeVariable
        return $ Constraint className classParam

-- Parser for types.
type' :: ParserIO Type
type' = functionType

-- Parser for function types (e.g. a -> b)
functionType :: ParserIO Type
functionType = do
    from <- paramType
    to   <- P.optional (symbol "->" *> functionType)
    return $ case to of
        Nothing -> from
        Just to -> FunctionType from to

-- Parser for parametrized types (e.g. Maybe a, f a b c)
paramType :: ParserIO Type
paramType = polyParamType
        <|> concreteParamType 
        <|> atomicType
  where
    polyParamType :: ParserIO Type
    polyParamType = do
        t <- TypeVar <$> typeVariable
        args <- P.many (P.try atomicType) 
        return $ case args of
            [] -> TypeVariable t
            _  -> PolymorphicParamType t args
    concreteParamType :: ParserIO Type
    concreteParamType = do
        t <- atomicType
        case t of
            NonPrimType t' -> do
                args <- P.many (P.try atomicType)
                return $ case args of
                    [] -> t
                    _  -> ParamType t' args
            _ -> return t

-- Parser for types that are atomic (like type names or types in brackets)
atomicType :: ParserIO Type
atomicType = (P.try arrayType <?> "array type")
        <|> (P.try listType <?> "list type")
        <|> (PrimitiveType <$> P.try primType) 
        <|> (NonPrimType . TypeName <$> P.try typeName <?> "type name")
        <|> (TypeVariable . TypeVar <$> P.try typeVariable <?> "type variable")
        <|> P.try (P.between (symbol "(") (symbol ")") type')
        <|> tupleType <?> "tuple type"

-- Parser for arrays types (e.g. [<Int>], [[<Float>]])
arrayType :: ParserIO Type
arrayType = ArrayType <$> (symbol "[<" *> type' <* symbol ">]")

-- Parser for tuples of types. They must be at least of length 2.
tupleType :: ParserIO Type
tupleType = TupleType <$> separatedList "(" ")" type' ","

-- Parser for list type syntax sugar. [] is a normal valid type name, but
-- there is a synonym [a] which is the same as ([] a)
listType :: ParserIO Type
listType = do
    t <- P.between (symbol "[") (symbol "]") type'
    return $ ParamType (TypeName "[]") [t]

-- Parser for primitive types like Int or ()
primType :: ParserIO PrimType
primType = keyword "Int" $> IntT
       <|> keyword "Float" $> FloatT
       <|> keyword "String" $> StringT
       <|> keyword "Bool" $> BoolT
       <|> (symbol "(" $> UnitT) <* symbol ")"
       <|> P.try (keyword "Char") $> CharT
       <|> keyword "CPtr" $> CPtrT

-- Parser for kind signatures (e.g. *, * -> *).
kindSignature :: ParserIO KindSig
kindSignature = do
    lhs <- atomicKind <?> "kind"
    rhs <- P.optional $ symbol "->" >> kindSignature
    return $ case rhs of
        Just rhs -> TypeConstructorKind lhs rhs
        Nothing  -> lhs
  where
    atomicKind :: ParserIO KindSig
    atomicKind = P.between (symbol "(") (symbol ")") kindSignature
             <|> (symbol "*" $> TypeKind)


{-# LANGUAGE OverloadedStrings #-}
module Parser.Definition (topLevelDef) where

import Parser.ParserIO
import Parser.Common
import Parser.Type
import Parser.Identifier
import Parser.Expression
import Parser.Pattern
import Syntax
import Utility (Position(..))
import Control.Monad (mapM_, when)
import Control.Applicative ((<|>))
import Text.Megaparsec ((<?>))
import Data.Maybe (maybeToList)
import Data.Functor (($>))
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as P
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- Parser for top-level definitions (types, classes, instances, let, etc.)
topLevelDef :: ParserIO (Maybe (TopLevelDef ()))
topLevelDef = (operatorDecl $> Nothing <?> "operator declaration")
    <|> (Just . TopLevelLet <$> letDef <?> "let definition")
    <|> (Just . TopLevelLet <$> letRecDef <?> "let-rec definition")
    <|> (Just . AliasDef    <$> typeAliasDef <?> "type alias definition")
    <|> (Just . TypeDef     <$> typeDef <?> "type definition")
    <|> (Just . ClassDef    <$> classDef <?> "class definition")
    <|> (Just . InstanceDef <$> instanceDef <?> "instance definition")

-- Parser for type definitions. All types must have at least one constructor.
typeDef :: ParserIO TDef
typeDef = withPos $ \pos -> do
    keyword "type"
    tName <- (TypeName <$> typeName) <?> "type name"
    params <- P.many (TypeVar <$> typeVariable <?> "type variable")
    kindSig <- P.optional (symbol ":" *> kindSignature) <?> "kind signature"
    keyword "with"
    constructors <- P.some typeDefCase
    registerConstructors tName constructors
    return $ TDef tName params kindSig constructors pos
  where
    -- Parser for a single constructor in type definition (e.g. case Nothing).
    typeDefCase :: ParserIO TypeCase
    typeDefCase = withPos $ \pos -> do
        keyword "case"
        constrName <- constructorName
        record constrName pos <|> normalConstructor constrName pos
      where
        -- Parser for records (e.g. Person { name : String, age : Int })
        record :: ConstructorName -> Position -> ParserIO TypeCase
        record name pos = do
            members <- separatedList "{" "}" recordMember ","
            return $ TypeCaseRecord name (RecordType members) pos
          where
            recordMember :: ParserIO RecordField
            recordMember = do
                memberName <- Identifier <$> prefixIdentifier
                symbol ":"
                RecordField memberName <$> type'
        -- Parser for normal constructor definitions (e.g. Just a)
        normalConstructor :: ConstructorName -> Position -> ParserIO TypeCase
        normalConstructor name pos = do
            params <- P.many (atomicType <?> "constructor parameter")
            return $ TypeCase name params pos
    -- Add all type constructors to the parser state so that later they can
    -- be implicitly exported.
    registerConstructors :: TypeName -> [TypeCase] -> ParserIO ()
    registerConstructors t cases = do
        stateOps <- stateCurrentOps <$> getState
        let constructors = filter (isOp $ concat stateOps) cases'
        mapM_ (registerConstructor t) constructors
      where
        cases' = map caseConstructorName cases
        caseConstructorName (TypeCaseRecord (ConstructorName name) _ _) = name
        caseConstructorName (TypeCase (ConstructorName name) _ _ ) = name
    -- Add a single constructor to the parser state
    registerConstructor :: TypeName -> T.Text -> ParserIO ()
    registerConstructor t name = do
        state <- getState
        let typeOpsMap = stateLocalTypeOps state
            prevConstructors = Map.lookup t typeOpsMap
            prevConstructorsList = concat $ maybeToList prevConstructors
            newList = ConstrOperator name : prevConstructorsList
        putState $ state { stateLocalTypeOps = Map.insert t newList typeOpsMap }

classDef :: ParserIO Class
classDef = do 
    keyword "class" 
    -- TODO: implement someday
    fail "type classes are not supported yet :("

instanceDef :: ParserIO (Instance ())
instanceDef = do
    keyword "instance"
    -- TODO: implement someday
    fail "type classes are not supported yet :("

-- Parser for type aliases.
typeAliasDef :: ParserIO TAlias
typeAliasDef = withPos $ \pos -> do
    keyword "alias"
    tName <- (TypeName <$> typeName) <?> "type name"
    params <- P.many (TypeVar <$> typeVariable) <?> "type parameters"
    symbol ":"
    kindSig <- P.optional kindSignature
    symbol "="
    aliasedType <- type'
    return $ TAlias tName params kindSig aliasedType pos

-- Parser for operator declarations.
operatorDecl :: ParserIO ()
operatorDecl = withPos $ \pos -> do
    P.try $ keyword "let-infix"
    op <- customOperator <?> "infix identifier"
    priority <- lexeme L.decimal <?> "operator precedence"
    fixity <- operatorFixity <?> "operator fixity (left, none or right)"
    assertValidPriority priority
    defineOperator (op, priority, fixity) pos
  where
    assertValidPriority :: Priority -> ParserIO ()
    assertValidPriority priority =
        when (priority < 1 || priority > 10) $
            fail $ "precedence out of range (got " 
                ++ show priority 
                ++ ", expected 1..10)"
    -- Parser for operator fixity in operator declarations.
    operatorFixity :: ParserIO Fixity
    operatorFixity = (keyword "left" $> LeftFix) 
                <|> (keyword "right" $> RightFix) 
                <|> (keyword "none"  $> NoneFix)
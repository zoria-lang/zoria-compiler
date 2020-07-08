{-# LANGUAGE OverloadedStrings #-}
module Parser.Identifier where

import Parser.ParserIO
import Parser.Common
import Syntax
import Utility (Position(..))
import Control.Monad (when)
import Text.Megaparsec ((<?>))
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Data.Map.Strict as Map

-- Parser for uppercase identifiers (e.g. Nothing, Maybe)
uppercaseName :: ParserIO T.Text
uppercaseName = lexeme $ do
    name <- T.pack <$> (pure (:) <*> P.upperChar <*> P.many nameChar)
    when (name `elem` upperReserved) $
        fail ("unexpected keyword " ++ show name)
    return name

-- Parser for lowercase identifiers (e.g. map)
lowercaseName :: ParserIO T.Text 
lowercaseName = lexeme $ do
    name <- T.pack <$> (pure (:) <*> P.lowerChar <*> P.many nameChar)
    when (name `elem` reserved) $
        fail ("unexpected keyword " ++ show name)
    return name

-- Parser for keywords. It makes sure the keyword is not a part of a longer
-- identifier.
keyword :: T.Text -> ParserIO ()
keyword kw = keywordParserIO <?> ("keyword " ++ show kw)
  where
    keywordParserIO = (lexeme . P.try) $ P.string kw *> P.notFollowedBy nameChar

-- Parser for 'let' keyword. Normal keyword parser won't suffice since
-- 'let' can be followed by '-'.
letKeyword :: ParserIO ()
letKeyword = keyword "let" >> P.notFollowedBy (P.char '-')

-- List of lowercase words that can not be used as identifiers.
reserved :: [T.Text]
reserved = ["module", "import", "class", "instance", "let", "in", "with",
            "match", "case", "and", "or", "fn", "type", "alias", "let-rec",
            "end", "if", "then", "else", "_external", "_internal", "λ", 
            "let-infix"]

-- List of uppercase words that can not be used as identifiers.
upperReserved :: [T.Text]
upperReserved = ["True", "False"]

-- Parser for a single character that is allowed in identifiers.
nameChar :: ParserIO Char
nameChar = P.choice [P.alphaNumChar, P.char '\'', P.char '_']

-- Parser for constructor names (e.g. (::), Just, [])
constructorName :: ParserIO ConstructorName
constructorName = ConstructorName <$> 
    (uppercaseName <|> surroundedBy "(" constructorOperator ")" <|> symbol "[]")

-- Parser for identifiers that can be appear in the prefix position 
-- (normal operators in brackets or variable names)
prefixIdentifier :: ParserIO T.Text
prefixIdentifier = lowercaseName 
               <|> (surroundedBy "(" operator ")")

-- Parser for qualified module names (e.g. Data\Map)
qualifiedModuleName :: ParserIO ModuleId
qualifiedModuleName = do
    prefix <- P.many $ P.try $ (uppercaseName) <* sep
    name   <- uppercaseName
    return $ ModuleId (ModName <$> prefix) (ModName name) 
  where
    sep = P.char '\\' <?> "scope operator \"\\\""

-- Constructor name constant for the nil constructor.
nilConstructor :: ConstructorName
nilConstructor = ConstructorName "[]"

-- Constructor name constant for the cons operator.
consConstructor :: ConstructorName
consConstructor = ConstructorName "::"

-- Extract the textual representation of a custom operator
unwrapOperator :: CustomOperator -> T.Text
unwrapOperator (ConstrOperator op) = op
unwrapOperator (PrefixConstrOperator op) = op
unwrapOperator (VarOperator op) = op
unwrapOperator (PrefixVarOperator op) = op

-- Parser for operators that don't start with ':'.
operator :: ParserIO T.Text
operator = lexeme $ do
    op <- T.pack <$> (pure (:) <*> operatorChar <*> P.many operatorCharOrColon)
    when (op `elem` reservedOperators) $
        fail ("unexpected reserved operator " ++ show op)
    return op

-- Parser for a character that is allowed within non-constructor operators.
operatorChar :: ParserIO Char
operatorChar = P.oneOf operatorCharsList

-- List of allowed operator characters. Note that it does not include ':'.
operatorCharsList :: [Char]
operatorCharsList = ['=', '+', '-', '*', '.', '/', '!', '~', '^',
                     '$', '%', '&', '?', '>', '<', '@', '|']

-- Parser for operator characters extended with ':' character.
operatorCharOrColon :: ParserIO Char
operatorCharOrColon = operatorChar <|> P.char ':'

-- List of built-in operators that can't be used as custom operators.
reservedOperators :: [T.Text]
reservedOperators = [":", "=>", "->", "@", "=", ":="]

-- Function which adds an operator declaration to the local operator table.
-- It takes the position of the operator declaration for better error handling.
defineOperator :: (CustomOperator, Priority, Fixity) -> Position -> ParserIO ()
defineOperator (op, priority, fixity) pos = do
    state @ ParserState { stateCurrentOps = visible } <- getState
    let previousOps   = concat $ Map.lookup (priority, fixity) visible
        updatedTable  = Map.insert (priority, fixity) (op : previousOps) visible
        updatedLocals = op : (stateLocalOps state)
    assertUndefined op
    putState $  state { stateCurrentOps = updatedTable 
                      , stateLocalOps   = updatedLocals
                      }
  where
    -- Assert that we are not overwritting some other custom operator.
    assertUndefined :: CustomOperator -> ParserIO ()
    assertUndefined op = do
        ops <- currentOperators
        when (op `elem` ops) $ posFail pos $ 
            "illegal redeclaration of the operator " ++ prettyPrintCustomOp op
    -- Extract all operators that are visible in the current module (this
    -- is different than operators *defined* in the current module).
    currentOperators :: ParserIO [CustomOperator]
    currentOperators = do
        table <- stateCurrentOps <$> getState
        return . concat . Map.elems $ table

-- Get the operators with given priority and fixity that are defined in
-- the current environment.
getOperators :: Priority -> Fixity -> ParserIO [CustomOperator]
getOperators priority fixity = do
    table <- stateCurrentOps <$> getState
    return . concat $ Map.lookup (priority, fixity) table

-- Pretty prints a custom operator.
prettyPrintCustomOp :: CustomOperator -> String
prettyPrintCustomOp (PrefixConstrOperator op) = '`' : T.unpack op ++ "`"
prettyPrintCustomOp (ConstrOperator op) = '(' : T.unpack op ++ ")"
prettyPrintCustomOp (PrefixVarOperator op) = '`' : T.unpack op ++ "`"
prettyPrintCustomOp (VarOperator op) = '(' : T.unpack op ++ ")"

-- Check whether some custom operator is a prefix operator (e.g. `elem`)
isPrefixOp :: CustomOperator -> Bool
isPrefixOp (PrefixConstrOperator _) = True
isPrefixOp (PrefixVarOperator _)    = True
isPrefixOp _ = False

-- Checks whether some custom operators is an infix operator (e.g. ::, >>=)
isInfixOp :: CustomOperator -> Bool
isInfixOp = not . isPrefixOp

-- Checks whether some custom operator is a constructor operator
isConstructorOp :: CustomOperator -> Bool
isConstructorOp (PrefixConstrOperator _) = True
isConstructorOp (ConstrOperator _) = True
isConstructorOp _ = False

-- Checks whether some custom operator is a normal operator
isNormalOp :: CustomOperator -> Bool
isNormalOp = not . isConstructorOp

-- Parser for operators starting with ':' except the ':' operator.
-- These operators can be used as constructor names only.
constructorOperator :: ParserIO T.Text
constructorOperator = lexeme $ do
    op <- T.pack <$> (pure (:) <*> P.char ':' <*> P.some operatorCharOrColon)
    when (op `elem` reservedOperators) $
        fail ("unexpected reserved operator " ++ show op)
    return op

-- Function which given a predicate on custom operators creates a parser that
-- matches this kind of operators with the given priority and fixity.
kindOfOperator :: (CustomOperator -> Bool) 
                -> Priority 
                -> Fixity 
                -> ParserIO T.Text
kindOfOperator kind priority fixity = do
    -- TODO: try to parse every operator to be able to give "undefined" error
    operators <- filter kind <$> getOperators priority fixity
    let infixOps  = unwrapOperator <$> filter isInfixOp operators
        prefixOps = unwrapOperator <$> filter isPrefixOp operators
    infixOp infixOps <|> P.try (prefixOp prefixOps)
  where
    infixOp :: [T.Text] -> ParserIO T.Text
    infixOp ops = P.choice (map operatorSymbol ops) <|> undefinedOperator
      where
        operatorSymbol :: T.Text -> ParserIO T.Text
        operatorSymbol op = 
            P.try (symbol op <* P.notFollowedBy operatorCharOrColon)
        undefinedOperator :: ParserIO T.Text
        undefinedOperator = withPos $ \pos -> do
            ops <- stateLocalOps <$> getState
            op <- P.try (cond (\op -> not (op `elem` ops)) customOperator)
            defineDefaultOperator op pos
            return . unwrapOperator $ op
        defineDefaultOperator :: CustomOperator -> Position -> ParserIO ()
        defineDefaultOperator op = defineOperator (op, 5, LeftFix)
    prefixOp :: [T.Text] -> ParserIO T.Text
    prefixOp ops = P.char '`' *> P.choice (map P.string ops) <* symbol "`"


-- Parser for constructor operators (e.g. ::, `Foo`)
constructorOperator' :: Priority -> Fixity -> ParserIO T.Text
constructorOperator' = kindOfOperator isConstructorOp

-- Parser for non-constructor operators (e.g. ++, `elem`)
operator' :: Priority -> Fixity -> ParserIO T.Text
operator' = kindOfOperator isNormalOp

-- Parser for operators in operator declarations.
customOperator :: ParserIO CustomOperator
customOperator = ConstrOperator <$> constructorOperator
            <|> VarOperator <$> operator
            <|> PrefixConstrOperator <$> 
                (P.try $ surroundedBy "`" uppercaseName "`")
            <|> PrefixVarOperator <$> surroundedBy "`" lowercaseName "`"

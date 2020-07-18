{-# LANGUAGE OverloadedStrings #-}
module Parser.Expression (expression, letDef, letRecDef) where

import Parser.ParserIO
import Parser.Common
import Parser.Identifier
import Parser.Type
import Parser.Primitive
import Parser.Pattern
import Syntax
import Control.Applicative ((<|>))
import Control.Monad (when)
import Text.Megaparsec ((<?>))
import Data.Functor (($>))
import Utility (Position(..))
import qualified Data.Text as T
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-- Parser for expressions.
expression :: ParserIO (Expr ())
expression = P.label "expression" $
            letIn
        <|> conditionalExpr
        <|> patternMatching 
        <|> lambdaExpr
        <|> annotatedExpr

-- Parser for 'let ... in' and 'let-rec ... in' expressions.
letIn :: ParserIO (Expr ())
letIn = do
    def <- letDef <|> letRecDef
    keyword "in"
    expr <- expression <?> "expression"
    return $ LetIn def expr ()

-- Parser for let-definitions
letDef :: ParserIO (LetDef ())
letDef = P.try letKeyword *> (LetDef <$> definition)

-- Parser for let-rec-definitions
letRecDef :: ParserIO (LetDef ())
letRecDef = withPos $ \pos -> do
    P.try (keyword "let-rec")
    defs <- (pure <$> definition) <|> (separatedList "{" "}" definition ";")
    return $ LetRecDef defs pos

-- Parser for the definition following the 'let' or 'let-rec' keywords.
-- Consists of the pattern, type signature and the expression.
definition :: ParserIO (Definition ())
definition = withPos $ \pos -> do
    patterns <- P.some (namedPattern <?> "pattern")
    symbol ":"
    sig <- P.optional typeSignature <?> "type signature"
    symbol "="
    body <- expression <?> "expression"
    case desugar patterns sig body pos of
        Left err  -> fail err
        Right def -> return def
  where
    -- Function which turns function definitions into lambda definitions.
    -- e.g. 'let f x := e' => 'let f := fn x => e'
    -- Function patterns must have a variable as the leading pattern.
    desugar [p] sig body pos = Right $ Definition p sig body pos
    desugar (fun@(VarPattern (Identifier var) _ _):args) sig body pos =
        Right $ Definition fun sig (lambdify args) pos
      where
        lambdify [p]    = Lambda p body (Just var) pos ()
        lambdify (p:ps) = Lambda p (lambdify ps) (Just var) pos ()
    desugar _ _ _ _ = 
        Left "invalid function pattern (function name must be an variable)"

-- Parser for 'if ... then ... else' expressions.
conditionalExpr :: ParserIO (Expr ())
conditionalExpr = withPos $ \pos -> do
    P.try $ keyword "if"
    condition <- annotatedExpr
    keyword "then"
    consequence <- expression
    keyword "else"
    alternative <- expression
    return $ If condition consequence alternative pos ()

-- Parser for 'match ... with case ...' expressions
patternMatching :: ParserIO (Expr ())
patternMatching = withPos $ \pos -> do
    P.try $ keyword "match"
    expr <- annotatedExpr
    keyword "with"
    cases <- P.some matchCase
    return $ Match expr cases pos ()

-- Parser for a single 'case' pattern matching clause.
matchCase :: ParserIO (MatchCase ())
matchCase = do
    P.try $ keyword "case"
    p <- pattern' <?> "pattern"
    keyword "=>"
    expr <- expression <?> "expression"
    return $ MatchCase p expr

-- Parser for lambda expressions (e.g. fn x => x)
lambdaExpr :: ParserIO (Expr ())
lambdaExpr = withPos $ \pos -> do
    P.try $ (keyword "fn" <|> keyword "λ")
    args <- P.some pattern' <?> "argument patterns"
    symbol "=>"
    body <- expression <?> "body expression"
    return $ desugar args body pos
  where
    -- Function that desugars multi-parameter lambdas into unary lambdas
    desugar :: [Pattern ()] -> Expr () -> Position -> Expr ()
    desugar [arg] expr pos = Lambda arg expr Nothing pos ()
    desugar (arg:args) expr pos = 
        Lambda arg (desugar args expr pos) Nothing pos ()

-- Parser for expressions with explicit type sygnatures or for expressions
-- with higher precedence.
annotatedExpr :: ParserIO (Expr ())
annotatedExpr = withPos $ \pos -> do
    expr <- logicalOr <?> "expression"
    sig  <- P.hidden $ P.optional $ (P.try $ symbol ":") >> typeSignature
    return $ case sig of
        Nothing  -> expr
        Just sig -> AnnotatedExpr expr sig pos ()

-- Parser for expressions with precedence at least the same as the precedence
-- of 'or' expressions.
logicalOr :: ParserIO (Expr ())
logicalOr = do
    lhs <- logicalAnd
    logicalOrAux lhs
  where
    logicalOrAux :: Expr () -> ParserIO (Expr ())
    logicalOrAux acc = withPos $ \pos -> do
        hasOp <- check $ symbol "or"
        if not hasOp
            then return acc
            else do 
                rhs <- logicalAnd <?> "expression"
                logicalOrAux $ Or acc rhs pos ()

-- Parser for expressions with precedence at least as high as 'and' expressions.
logicalAnd :: ParserIO (Expr ())
logicalAnd = do
    lhs <- opExpr 10 NoneFix
    logicalAndAux lhs
  where
    logicalAndAux :: Expr () -> ParserIO (Expr ())
    logicalAndAux acc = withPos $ \pos -> do
        hasOp <- check $ symbol "and"
        if not hasOp
            then return acc
            else do
                rhs <- opExpr 10 NoneFix <?> "expression"
                logicalAndAux $ And acc rhs pos ()

-- Parser for any kind of infix operator which returns the operator
-- as either Var or Constructor expression.
exprOperator :: Priority -> Fixity -> ParserIO (Expr ())
exprOperator priority fixity = P.label "infix operator" $
    P.try constrOp <|> normalOp
  where
    normalOp :: ParserIO (Expr ())
    normalOp = withPos $ \pos -> do
        op <- kindOfOperator isNormalOp priority fixity
        return $ Var (Identifier op) pos ()
    constrOp :: ParserIO (Expr ())
    constrOp = withPos $ \pos -> do
        op <- kindOfOperator isConstructorOp priority fixity
        return $ Constructor (ConstructorName op) pos ()

-- Parser for expressions with (custom) operators.
-- There is a lot of repetition but it's not possible to abstract into a
-- single definition since there are subtle differences between clauses.
opExpr :: Priority -> Fixity -> ParserIO (Expr ())
opExpr 0 _ = application
opExpr priority NoneFix = do
    lhs <- nextPriorityExpr <?> "expression"
    rhs <- P.optional $ pure (,)
        <*> (exprOperator priority NoneFix <?> "operator")
        <*> (nextPriorityExpr <?> "expression")
    return $ case rhs of
        Just (op, rhs) -> App (App op lhs ()) rhs ()
        Nothing        -> lhs
  where
    nextPriorityExpr = opExpr priority RightFix
opExpr priority RightFix = do
    lhs <- opExpr priority LeftFix <?> "expression"
    rhs <- P.optional $ pure (,)
        <*> (exprOperator priority RightFix <?> "operator")
        <*> (opExpr priority RightFix <?> "expression")
    return $ case rhs of
        Just (op, rhs) -> App (App op lhs ()) rhs ()
        Nothing        -> lhs
opExpr priority LeftFix = do
    lhs <- nextPriorityExpr <?> "expression"
    leftfixExprAux lhs
  where
    nextPriorityExpr = opExpr (priority - 1) NoneFix
    leftfixExprAux :: Expr () -> ParserIO (Expr ())
    leftfixExprAux acc = do
        op <- (P.optional $ exprOperator priority LeftFix) <?> "operator"
        case op of
            Nothing -> return acc
            Just op -> do
                rhs <- nextPriorityExpr <?> "expression"
                leftfixExprAux $ App (App op acc ()) rhs ()

-- Parser for function application. Function application can be treated like
-- invisible leftfix operator with priority 0 (highest).
application :: ParserIO (Expr ())
application = do
    head <- atomicExpr <?> "expression"
    args  <- P.many atomicExpr <?> "argument expressions"
    return $ case args of
        [] -> head
        _  -> foldl applify head args
  where
    applify :: Expr () -> Expr () -> Expr ()
    applify funct arg = App funct arg ()

-- Parser for expressions with the highest precedence (literals / bracketed
-- expressions)
atomicExpr :: ParserIO (Expr ())
atomicExpr = withPos $ \pos -> 
    ((\e -> Primitive e pos ()) <$> P.try primExpr)
    <|> internal
    <|> external
    <|> stringExpr
    <|> P.try listLit
    <|> arrayLit
    <|> blockExpr
    <|> P.try qualifiedName
    <|> P.try varExpr
    <|> P.try constructorExpr
    <|> tupleExpr

-- Returns a readable error in case of an undefined operator.
operatorFail :: ParserIO a
operatorFail = do
    op <- unwrapOperator <$> P.try customOperator
    fail $ "unknown operator " ++ show op 

-- Used by blockExpr parser.
data BlockElem
    = BlockLet (LetDef ()) Position
    | BlockExpr (Expr ()) Position

-- Parser for block expressions.
blockExpr :: ParserIO (Expr ())
blockExpr = withPos $ \pos -> do
    symbol "{"
    exprs <- P.many $ (blockLet <* symbol ";") 
                  <|> P.try (blockExpr <* symbol ";")
    final <- withPos $ \pos -> P.option (implicitUnit pos) (P.try blockExpr)
    symbol "}"
    return $ Block (desugar $ exprs ++ [final]) pos ()
  where
    -- Parser for block let definitions.
    blockLet :: ParserIO BlockElem
    blockLet = withPos $ \pos -> 
        (flip BlockLet pos) <$> (letDef <|> letRecDef) <?> "let definition"
    -- Parser for block expressions wrapped in the BlockElem type.
    blockExpr :: ParserIO BlockElem
    blockExpr = withPos $ \pos -> 
        (flip BlockExpr pos) <$> expression <?> "expression"
    -- Impliit unit placed after the final trailing semicolon in the block.
    implicitUnit :: Position -> BlockElem
    implicitUnit pos = BlockExpr (Primitive UnitLit pos ()) pos
    -- Function that turns a linear list of expressions into a proper tree.
    desugar :: [BlockElem] -> [Expr ()]
    desugar [] = []
    desugar ((BlockLet def pos):exprs) = [LetIn def subBlock ()]
      where
        subBlock = Block (desugar exprs) pos ()
    desugar ((BlockExpr expr pos):exprs) = expr : desugar exprs

-- Parsers for names with explicit namespace (e.g. Foo\bar, Bar\Foo, Foo\(++))
qualifiedName :: ParserIO (Expr ())
qualifiedName = withPos $ \pos -> do
    namespace <- ModName <$> (P.try $ uppercaseName <* P.char '\\')
    P.try (qualifiedConstructor namespace pos) <|> qualifiedVar namespace pos
  where
    qualifiedConstructor :: ModName -> Position -> ParserIO (Expr ())
    qualifiedConstructor mod pos = do 
        name <- constructorName
        return $ QualifiedConstructor mod name pos ()
    qualifiedVar :: ModName -> Position -> ParserIO (Expr ())
    qualifiedVar mod pos = do
        name <- Identifier <$> prefixIdentifier
        return $ QualifiedVar mod name pos ()

-- Parser for identifiers (e.g. x, (++))
varExpr :: ParserIO (Expr ())
varExpr = withPos $ \pos -> do
    var <- Identifier <$> prefixIdentifier
    return $ Var var pos ()

-- Parser for identifiers that are constructors (e.g. Nothing, (::))
constructorExpr :: ParserIO (Expr ())
constructorExpr = withPos $ \pos -> do
    constr <- constructorName
    return $ Constructor constr pos ()

-- Parser for tuples of expressions (e.g. (2,'1',"3",7.0))
tupleExpr :: ParserIO (Expr ())
tupleExpr = withPos $ \pos -> do
    elems <- separatedList "(" ")" expression ","
    return $ case elems of
        [expr] -> expr
        _      -> Tuple elems pos ()

-- Parser for array literals (e.g. [<1,2,3>])
arrayLit :: ParserIO (Expr ())
arrayLit = withPos $ \pos -> do
    elems <- separatedList "[<" ">]" expression ","
    return $ Array elems pos ()

-- Parser for both normal and format strings.
stringExpr :: ParserIO (Expr ())
stringExpr = withPos $ \pos -> unwrap pos . filterEmpty <$> formatString 
  where
    unwrap :: Position -> [FormatExpr ()] -> Expr ()
    unwrap pos [] = Primitive (StringLit "") pos ()
    unwrap pos [FmtStr str] = Primitive (StringLit str) pos ()
    unwrap pos chunks = FormatString chunks pos ()
    formatString :: ParserIO [FormatExpr ()]
    formatString = 
        P.char '"' *> P.some (inlineExpr <|> stringChunk) <* symbol "\""
    stringChunk :: ParserIO (FormatExpr ())
    stringChunk = FmtStr . T.pack <$> P.some chunkChar
    chunkChar :: ParserIO Char
    chunkChar = 
        P.lookAhead (P.satisfy (\c -> c /= '{' && c /= '"')) *> L.charLiteral
    inlineExpr :: ParserIO (FormatExpr ())
    inlineExpr = 
        FmtExpr <$> (symbol "{" *> (expression <?> "expression") <* P.char '}')
    filterEmpty :: [FormatExpr ()] -> [FormatExpr ()]
    filterEmpty = filter notEmptyStr
      where
        notEmptyStr (FmtStr "") = False
        notEmptyStr _           = True

-- Parser for list literal expressions. Note, that they are just a syntax
-- sugar for :: and [] constructors.
listLit :: ParserIO (Expr ())
listLit = withPos $ \pos -> do
    xs <- separatedList "[" "]" expression ","
    return $ foldr (applify pos) (Constructor nilConstructor pos ()) xs
  where
    applify :: Position -> Expr () -> Expr () -> Expr ()
    applify pos x xs = 
        App (App (Constructor consConstructor pos ()) x ()) xs ()

-- Parser for the '_internal' built-in names.
internal :: ParserIO (Expr ())
internal = withPos $ \pos -> do
    P.try $ keyword "_internal"
    id <- Identifier <$> lowercaseName
    return $ Internal id pos ()

-- Parser for the '_external' FfI calls.
external :: ParserIO (Expr ())
external = withPos $ \pos -> do
    P.try $ keyword "_external"
    path <- T.unpack <$> string
    name <- string
    return $ External path name pos ()


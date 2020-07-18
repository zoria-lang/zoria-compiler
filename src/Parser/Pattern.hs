{-# LANGUAGE OverloadedStrings #-}
module Parser.Pattern (pattern', namedPattern) where

import Parser.ParserIO
import Parser.Identifier
import Parser.Common
import Parser.Primitive
import Syntax
import Utility (Position(..))
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Text.Megaparsec as P

-- Parser for patterns which can appear either in 'match ... with' 
-- or let definitions
pattern' :: ParserIO (Pattern ())
pattern' = infixConstructorPattern 10 NoneFix

-- Infix application of a constructor (e.g. a `Foo` b, x::xs)
infixConstructorPattern :: Priority -> Fixity -> ParserIO (Pattern ())
infixConstructorPattern 0 _ = prefixConstructorPattern
infixConstructorPattern priority NoneFix = withPos $ \pos -> do
    lhs <- infixConstructorPattern priority RightFix
    rhs <- P.optional $ ((,) <$> P.try (constructorOperator' priority NoneFix))
                             <*> infixConstructorPattern priority RightFix
    return $ case rhs of
        Just (op, rhs) -> 
            ConstructorPattern (ConstructorName op) [lhs, rhs] pos ()
        Nothing -> lhs
infixConstructorPattern priority RightFix = withPos $ \pos -> do
    lhs <- infixConstructorPattern priority LeftFix
    rhs <- P.optional $ ((,) <$> P.try (constructorOperator' priority RightFix))
                             <*> infixConstructorPattern priority RightFix
    return $ case rhs of
        Just (op, rhs) -> 
            ConstructorPattern (ConstructorName op) [lhs, rhs] pos ()
        Nothing -> lhs
infixConstructorPattern priority LeftFix = withPos $ \pos -> do
    lhs <- infixConstructorPattern (priority - 1) NoneFix
    leftfixConstructorPattern priority lhs
  where
    -- Tail recursive parser for left-recursion elimination.
    leftfixConstructorPattern :: Priority -> Pattern () -> ParserIO (Pattern ())
    leftfixConstructorPattern priority acc = withPos $ \pos -> do
        op <- P.optional $ constructorOperator' priority LeftFix
        case op of
            Nothing -> return acc
            Just op -> do
                rhs <- infixConstructorPattern (priority - 1) NoneFix
                leftfixConstructorPattern priority $
                    ConstructorPattern (ConstructorName op) [acc, rhs] pos ()

-- Prefix application of a constructor (e.g. Just 42, (::) x xs)
prefixConstructorPattern :: ParserIO (Pattern ())
prefixConstructorPattern = P.try constructorPattern <|> namedPattern
  where
    constructorPattern :: ParserIO (Pattern ())
    constructorPattern = withPos $ \pos -> do
        constructor <- constructorName
        arguments <- P.many namedPattern
        return $ ConstructorPattern constructor arguments pos ()

-- Parser for synonym patterns (e.g. ys@(x::xs))
namedPattern :: ParserIO (Pattern ())
namedPattern = P.try namedPattern' <|> atomicPattern
  where
    namedPattern' :: ParserIO (Pattern ())
    namedPattern' = withPos $ \pos -> do
        name <- Identifier <$> prefixIdentifier
        symbol "@"
        pat <- atomicPattern
        return $ NamedPattern name pat pos ()

-- Parser for list literals. List literals are just syntax sugar for
-- :: operator and [] constructor.
listLiteralPattern :: ParserIO (Pattern ())
listLiteralPattern = withPos $ \pos -> do
    (Located pos ps) <- located $ separatedList "[" "]" pattern' ","
    return $ foldPatterns ps pos
  where
    foldPatterns :: [Pattern ()] -> Position -> Pattern ()
    foldPatterns [] pos = ConstructorPattern nilConstructor [] pos ()
    foldPatterns (p:ps) pos = 
        ConstructorPattern consConstructor [p, ps'] pos ()
      where
        ps' = foldPatterns ps pos

-- Pattern expression with the highest precedence or any pattern in parenthesis.
atomicPattern :: ParserIO (Pattern ())
atomicPattern = lexeme $ 
    wildcardPattern
    <|> P.try literalPattern
    <|> P.try variablePattern
    <|> P.try (surroundedBy "(" pattern' ")")
    <|> listLiteralPattern
    <|> tuplePattern

-- Tuple of patterns (e.g. (1,2), ())
tuplePattern :: ParserIO (Pattern ())
tuplePattern = withPos $ \pos -> do
    subpatterns <- separatedList "(" ")" pattern' ","
    guard (length subpatterns > 1)
    return $ TuplePattern subpatterns pos ()

-- Parser that parses patterns consisting of variables
variablePattern :: ParserIO (Pattern ())
variablePattern = withPos $ \pos -> do
    var <- Identifier <$> prefixIdentifier
    return $ VarPattern var pos ()

-- Parser for the '_' pattern.
wildcardPattern :: ParserIO (Pattern ())
wildcardPattern = withPos $ \pos -> symbol "_" $> WildcardPattern pos ()

-- Parser for constant patterns (e.g. 42, "string", True)
literalPattern :: ParserIO (Pattern ())
literalPattern = withPos $ \pos -> do
    expr <- primExpr
    return $ ConstPattern expr pos ()

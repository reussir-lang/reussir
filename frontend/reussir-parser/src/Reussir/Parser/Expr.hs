{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Reussir.Parser.Expr where

import Control.Monad.Combinators.Expr

import Data.Functor
import Data.Maybe

import Data.Text qualified as T
import Reussir.Parser.Lexer
import Reussir.Parser.Type (parseType)
import Reussir.Parser.Types
import Reussir.Parser.Types.Expr

parseBody :: Parser Expr
parseBody = openBody *> parseExpr <* closeBody

parsePattern :: Parser Pattern
parsePattern = do
    ns <- parseIdentifier
    name <- doubleColon *> parseIdentifier
    args <- optional $ openParen *> parseIdentifier `sepBy` comma <* closeParen

    return (Pattern ns name (fromMaybe [] args))

parseIf :: Parser Expr
parseIf = do
    cond <- string "if" *> space *> parseExpr
    iftrue <- parseBody
    iffalse <- string "else" *> space *> parseBody

    return (If cond iftrue iffalse)

parseLetIn :: Parser Expr
parseLetIn = do
    name <- string "let" *> space *> parseIdentifier
    value <- char '=' *> space *> parseExpr <* semicolon
    body <- parseExpr

    return (LetIn name value body)

parseFuncCall :: Parser Expr
parseFuncCall = do
    name <- parsePath
    args <- openParen *> parseExpr `sepBy` comma <* closeParen
    return (FuncCall name args)

parseLambda :: Parser Expr
parseLambda = do
    name <- char '|' *> space *> parseIdentifier
    ty <- colon *> parseType <* char '|' <* space
    body <- parseExpr

    return (Lambda name ty body)

parseMatchCase :: Parser (Pattern, Expr)
parseMatchCase = do
    pat <- parsePattern
    expr <- string "=>" *> space *> parseExpr
    return (pat, expr)

parseMatch :: Parser Expr
parseMatch = do
    expr <- string "match" *> space *> parseExpr
    body <- openBody *> parseMatchCase `sepBy` comma <* closeBody
    return (Match expr body)

parseConstant :: Parser Constant
parseConstant =
    try (ConstInt <$> parseInt)
        <|> (ConstDouble <$> parseDouble)
        <|> (ConstString <$> parseString)
        <|> (ConstBool <$> parseBool)

prefixOp :: T.Text -> UnaryOp -> Operator Parser Expr
prefixOp symbol op = Prefix (string symbol *> space $> UnaryOpExpr op)

infixLOp :: T.Text -> BinaryOp -> Operator Parser Expr
infixLOp symbol op = InfixL (string symbol *> space $> BinOpExpr op)

infixNOp :: T.Text -> BinaryOp -> Operator Parser Expr
infixNOp symbol op = InfixN (string symbol *> space $> BinOpExpr op)

castOp :: Operator Parser Expr
castOp = Postfix $ do
    ty <- string "as" *> space *> parseType
    return (Cast ty)

exprOpTable :: [[Operator Parser Expr]]
exprOpTable =
    [
        [ prefixOp "-" Negate
        , prefixOp "!" Not
        , castOp
        ]
    ,
        [ infixLOp "*" Mul
        , infixLOp "/" Div
        , infixLOp "%" Mod
        ]
    ,
        [ infixLOp "+" Add
        , infixLOp "-" Sub
        ]
    ,
        [ infixNOp ">=" Gte
        , infixNOp "<=" Lte
        , infixNOp ">" Gt
        , infixNOp "<" Lt
        , infixNOp "==" Equ
        , infixNOp "!=" Neq
        ]
    ,
        [ infixLOp "&&" And
        ]
    ,
        [ infixLOp "||" Or
        ]
    ]

parseExprTerm :: Parser Expr
parseExprTerm =
    choice
        [ char '(' *> parseExpr <* char ')' <* space
        , parseIf
        , parseLetIn
        , parseMatch
        , try parseFuncCall
        , ConstExpr <$> parseConstant
        , Var <$> parsePath
        ]

parseExpr :: Parser Expr
parseExpr = SpannedExpr <$> withSpan parseExprInner

parseExprInner :: Parser Expr
parseExprInner = parseLambda <|> makeExprParser parseExprTerm exprOpTable

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Reussir.Parser.Expr where

import Control.Monad.Combinators.Expr

import Data.Functor
import Data.Maybe

import Data.Text qualified as T
import Reussir.Parser.Lexer
import Reussir.Parser.Type (parseType)
import Reussir.Parser.Types
import Reussir.Parser.Types.Expr
import Reussir.Parser.Types.Lexer (Identifier)
import Reussir.Parser.Types.Type (Type)

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
    ty <- optional (colon *> parseTypeWithCap)
    value <- char '=' *> space *> parseExpr <* semicolon
    body <- parseExpr

    return (LetIn name ty value body)
  where
    parseTypeWithCap = do
        cap <- parseCapability
        ty <- parseType
        return (ty, cap)

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

parseTypeArg :: Parser (Maybe Type)
parseTypeArg = (char '_' *> space $> Nothing) <|> (Just <$> parseType)

parseCtorArg :: Parser (Maybe Identifier, Expr)
parseCtorArg =
    try
        ( do
            id' <- parseIdentifier
            colon
            e <- parseExpr
            return (Just id', e)
        )
        <|> ( do
                e <- parseExpr
                return (Nothing, e)
            )

parseCtorArgs :: Parser [(Maybe Identifier, Expr)]
parseCtorArgs =
    choice
        [ openBody *> parseCtorArg `sepBy` comma <* closeBody
        , do
            exprs <- openParen *> parseExpr `sepBy` comma <* closeParen
            return $ map (Nothing,) exprs
        , pure []
        ]

parseRegionalExpr :: Parser Expr
parseRegionalExpr = do
    _ <- string "regional" *> space
    body <- parseBody
    return (RegionalExpr body)

parsePathBasedExpr :: Parser Expr
parsePathBasedExpr = do
    path <- parsePath
    tyArgs <- optional (openAngle *> parseTypeArg `sepBy` comma <* closeAngle)

    isVariant <- optional doubleColon

    case isVariant of
        Just _ -> do
            v <- parseIdentifier
            args <- parseCtorArgs
            return $ CtorCallExpr $ CtorCall path (Just v) (fromMaybe [] tyArgs) args
        Nothing -> do
            lookAheadChar <- optional (lookAhead anySingle)
            case lookAheadChar of
                Just '{' -> do
                    args <- openBody *> parseCtorArg `sepBy` comma <* closeBody
                    return $ CtorCallExpr $ CtorCall path Nothing (fromMaybe [] tyArgs) args
                Just '(' -> do
                    args <- openParen *> parseExpr `sepBy` comma <* closeParen
                    return $ FuncCallExpr $ FuncCall path (fromMaybe [] tyArgs) args
                _ -> do
                    case tyArgs of
                        Just ts -> return $ CtorCallExpr $ CtorCall path Nothing ts []
                        Nothing -> return $ Var path

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
        , parseRegionalExpr
        , parsePathBasedExpr
        , ConstExpr <$> parseConstant
        ]

parseExpr :: Parser Expr
parseExpr = SpannedExpr <$> withSpan parseExprInner

parseExprInner :: Parser Expr
parseExprInner = parseLambda <|> makeExprParser parseExprTerm exprOpTable

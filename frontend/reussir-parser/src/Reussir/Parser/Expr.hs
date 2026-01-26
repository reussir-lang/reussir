{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Reussir.Parser.Expr where

import Control.Monad.Combinators.Expr

import Data.Functor
import Data.Maybe

import Data.Text qualified as T
import Data.Vector.Strict (fromList)
import Reussir.Parser.Lexer
import Reussir.Parser.Type (parseType)
import Reussir.Parser.Types hiding (space)
import Reussir.Parser.Types.Expr
import Reussir.Parser.Types.Lexer (Identifier)
import Reussir.Parser.Types.Type (Type)

parseBody :: Parser Expr
parseBody = parseExprSeq

parseExprSeq :: Parser Expr
parseExprSeq = ExprSeq <$> (openBody *> parseExpr `sepEndBy` semicolon <* closeBody)

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
    name <- string "let" *> space *> withSpan parseIdentifier
    ty <- optional (colon *> parseTypeWithFlex)
    value <- char '=' *> space *> parseExpr
    return (Let name ty value)
  where
    parseTypeWithFlex = do
        flexFlag <- optional (char '[' *> space *> string "flex" <* space <* char ']' <* space)
        ty <- parseType
        return (ty, isJust flexFlag)

assignOp :: Operator Parser Expr
assignOp = InfixN $ do
    access <- space *> parseArrowAccess <* string ":=" <* space
    return $ \e1 e2 -> Assign e1 access e2

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
    return (Match expr (fromList body))

parseConstant :: Parser Constant
parseConstant =
    try (ConstInt <$> parseInt)
        <|> (ConstDouble <$> parseDouble)
        <|> (ConstString <$> parseString)
        <|> (ConstBool <$> parseBool)

parseTypeArg :: Parser (WithSpan (Maybe Type))
parseTypeArg = withSpan $ (char '_' *> space $> Nothing) <|> (Just <$> parseType)

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
    _ <- try (string "regional" *> space *> lookAhead (char '{'))
    body <- parseBody
    return (RegionalExpr body)

parsePathBasedExpr :: Parser Expr
parsePathBasedExpr = do
    path <- parsePath
    tyArgs <- optional (try (openAngle *> parseTypeArg `sepBy` comma <* closeAngle))
    lookAheadChar <- optional (lookAhead anySingle)
    case lookAheadChar of
        Just '{' -> do
            args <- openBody *> parseCtorArg `sepBy` comma <* closeBody
            return $ CtorCallExpr $ CtorCall path (fromMaybe [] tyArgs) args
        Just '(' -> do
            args <- openParen *> parseExpr `sepBy` comma <* closeParen
            return $ FuncCallExpr $ FuncCall path (fromMaybe [] tyArgs) args
        _ -> do
            case tyArgs of
                Just ts -> return $ CtorCallExpr $ CtorCall path ts []
                Nothing -> return $ Var path

prefixOp :: T.Text -> UnaryOp -> Operator Parser Expr
prefixOp symbol op = Prefix (string symbol *> space $> UnaryOpExpr op)

infixLOp :: T.Text -> BinaryOp -> Operator Parser Expr
infixLOp symbol op = case symbol of
    "-" ->
        -- need to specially handle the ambiguity with arrow assignment
        let sub = try $ string symbol *> notFollowedBy (char '>')
         in InfixL (sub *> space $> BinOpExpr op)
    _ -> InfixL (string symbol *> space $> BinOpExpr op)

infixNOp :: T.Text -> BinaryOp -> Operator Parser Expr
infixNOp symbol op = InfixN (string symbol *> space $> BinOpExpr op)

castOp :: Operator Parser Expr
castOp = Postfix $ do
    ty <- string "as" *> space *> parseType
    return (Cast ty)

parseAccess :: Parser Access
parseAccess =
    dot
        *> ( (Named <$> parseIdentifier)
                <|> (Unnamed . read <$> (some digitChar <* space))
           )

parseArrowAccess :: Parser Access
parseArrowAccess =
    arrow
        *> ( (Named <$> parseIdentifier)
                <|> (Unnamed . read <$> (some digitChar <* space))
           )

accessOp :: Operator Parser Expr
accessOp = Postfix $ do
    access <- some parseAccess
    return $ flip AccessChain (fromList access)

exprOpTable :: [[Operator Parser Expr]]
exprOpTable =
    [
        [ prefixOp "-" Negate
        , prefixOp "!" Not
        , castOp
        , accessOp
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
    ,
        [ assignOp
        ]
    ]

parseExprTerm :: Parser Expr
parseExprTerm =
    choice
        [ (char '(' *> space *> parseExpr <* space <* char ')' <* space) <?> "parenthesized expression"
        , parseIf <?> "if expression"
        , parseLetIn <?> "let binding"
        , parseMatch <?> "match expression"
        , parseRegionalExpr <?> "regional expression"
        , ConstExpr <$> parseConstant <?> "constant"
        , SpannedExpr <$> withSpan parsePathBasedExpr <?> "variable or function call"
        , parseExprSeq <?> "expression sequence"
        ]

parseExpr :: Parser Expr
parseExpr =
    (SpannedExpr <$> withSpan parseLambda)
        <|> makeExprParser (SpannedExpr <$> withSpan parseExprTerm) exprOpTable

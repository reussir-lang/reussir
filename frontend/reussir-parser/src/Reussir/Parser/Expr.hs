{-# LANGUAGE LambdaCase #-}

module Reussir.Parser.Expr where

import Control.Monad.Combinators.Expr

import Data.Functor
import Data.Maybe

import Reussir.Parser.Types
import Reussir.Parser.Types.Expr

colon :: Parser ()
colon = char ':' *> space

semicolon :: Parser ()
semicolon = char ';' *> space

doubleColon :: Parser ()
doubleColon = string "::" *> space

openParen :: Parser ()
openParen = char '(' *> space

closeParen :: Parser ()
closeParen = char ')' *> space

comma :: Parser ()
comma = char ',' *> space

openBody :: Parser ()
openBody = char '{' *> space

closeBody :: Parser ()
closeBody = char '}' *> space

openAngle :: Parser ()
openAngle = char '<' *> space

closeAngle :: Parser ()
closeAngle = char '>' *> space

rightArrow :: Parser ()
rightArrow = string "->" *> space

parseBody :: Parser Expr
parseBody = openBody *> parseExpr <* closeBody

parsePattern :: Parser Pattern
parsePattern = do
    ns <- parseIdentifier
    name <- doubleColon *> parseIdentifier
    args <- optional $ openParen *> parseIdentifier `sepBy` comma <* closeParen

    return (Pattern ns name (fromMaybe [] args))

parseTypenameTerm :: Parser Typename
parseTypenameTerm = do
    prefix <- fmap unIdentifier parseIdentifier
    suffix <- optional parseTypenameParams

    return (Typename prefix (fromMaybe [] suffix))

parseTypenameArrow :: Parser Typename
parseTypenameArrow = do
    a <- parseTypenameTerm <* rightArrow
    b <- parseTypenameTerm
    return (Arr a b)

parseTypename :: Parser Typename
parseTypename = try parseTypenameArrow <|> parseTypenameTerm

parseTypenameParams :: Parser [Typename]
parseTypenameParams = openAngle *> parseTypename `sepBy1` comma <* closeAngle

parseIdentifier :: Parser Identifier
parseIdentifier = do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_') <* space

    return $ Identifier (first : rest)

parseIntSuffix :: Parser ()
parseIntSuffix = choice [string ('u' : show @Int s) | s <- [8, 16, 32, 64]] *> space

parseFloatSuffix :: Parser ()
parseFloatSuffix = choice [string ('f' : show @Int s) | s <- [16, 32, 64]] *> space

parseInt :: Parser Int
parseInt = read <$> some digitChar <* optional parseIntSuffix <* space

parseDouble :: Parser Double
parseDouble = do
    first <- some digitChar
    rest <- char '.' *> some digitChar <* optional parseFloatSuffix <* space
    return $ read (first ++ "." ++ rest)

parseString :: Parser String
parseString = char '"' *> some asciiChar <* char '"' <* space

parseBool :: Parser Bool
parseBool =
    choice
        [ string "true" $> True
        , string "false" $> False
        ]
        <* space

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
    name <- parseIdentifier
    args <- openParen *> parseExpr `sepBy` comma <* closeParen
    return (FuncCall name args)

parseLambda :: Parser Expr
parseLambda = do
    name <- char '|' *> space *> parseIdentifier
    ty <- colon *> parseTypename <* char '|' <* space
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
    try (ConstDouble <$> parseDouble)
        <|> (ConstInt <$> parseInt)
        <|> (ConstString <$> parseString)
        <|> (ConstBool <$> parseBool)
        <|> (ConstID <$> parseIdentifier)

prefixOp :: String -> UnaryOp -> Operator Parser Expr
prefixOp symbol op = Prefix (string symbol *> space $> UnaryOpExpr op)

infixLOp :: String -> BinaryOp -> Operator Parser Expr
infixLOp symbol op = InfixL (string symbol *> space $> BinOpExpr op)

infixNOp :: String -> BinaryOp -> Operator Parser Expr
infixNOp symbol op = InfixN (string symbol *> space $> BinOpExpr op)

castOp :: Operator Parser Expr
castOp = Postfix $ do
    ty <- string "as" *> space *> parseTypename
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
        ]

parseExpr :: Parser Expr
parseExpr = parseLambda <|> makeExprParser parseExprTerm exprOpTable

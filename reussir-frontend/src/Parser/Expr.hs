{-# LANGUAGE LambdaCase #-}

module Parser.Expr where

import Control.Monad.Combinators.Expr

import Data.Functor

import Parser.Types
import Parser.Types.Expr

openBody :: Parser ()
openBody = char '{' *> space

closeBody :: Parser ()
closeBody = char '}' *> space

parseBody :: Parser Expr
parseBody = openBody *> parseExpr <* closeBody

parseTypename :: Parser Typename
parseTypename = fmap (\(Identifier name) -> Typename name) parseIdentifier

parseIdentifier :: Parser Identifier
parseIdentifier = do 
    first <- letterChar
    rest  <- many (alphaNumChar <|> char '_') <* space

    return $ Identifier (first : rest)

parseInt :: Parser Int 
parseInt = read <$> some digitChar <* space

parseDouble :: Parser Double
parseDouble = do 
    first <- some digitChar
    rest  <- char '.' *> some digitChar <* space
    return $ read (first ++ "." ++ rest)

parseString :: Parser String
parseString = char '"' *> some asciiChar <* char '"' <* space

parseBool :: Parser Bool
parseBool = choice
    [ string "true"  $> True
    , string "false" $> False
    ] <* space

parseIf :: Parser Expr 
parseIf = do 
    cond    <- string "if" *> space *> parseExpr
    iftrue  <- parseBody
    iffalse <- string "else" *> space *> parseBody

    return (If cond iftrue iffalse)

parseConstant :: Parser Constant
parseConstant = try (ConstDouble <$> parseDouble)
            <|>     (ConstInt    <$> parseInt)
            <|>     (ConstString <$> parseString)
            <|>     (ConstBool   <$> parseBool)
            <|>     (ConstID     <$> parseIdentifier)

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
exprOpTable = [ [ prefixOp "-" Negate
                , prefixOp "!" Not
                , castOp
                ]
              , [ infixLOp "*" Mul
                , infixLOp "/" Div
                ]
              , [ infixLOp "+" Add
                , infixLOp "-" Sub
                ]
              , [ infixNOp ">" Gt
                , infixNOp "<" Lt
                , infixNOp ">=" Gte
                , infixNOp "<=" Lte
                , infixNOp "==" Equ
                , infixNOp "!=" Neq
                ]
              , [ infixLOp "&&" And
                ]
              , [ infixLOp "||" Or
                ]
              ]

parseExprTerm :: Parser Expr 
parseExprTerm = choice
    [ char '(' *> parseExpr <* char ')' <* space
    , parseIf
    , ConstExpr <$> parseConstant
    ]

parseExpr :: Parser Expr
parseExpr = makeExprParser parseExprTerm exprOpTable

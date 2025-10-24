{-# LANGUAGE LambdaCase #-}

module Parser.Expr where

import Control.Monad.Combinators.Expr

import Data.Functor

import Parser.Types
import Parser.Types.Expr

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

parseConstant :: Parser Constant
parseConstant = try (ConstDouble <$> parseDouble)
            <|>     (ConstInt    <$> parseInt)
            <|>     (ConstString <$> parseString)
            <|>     (ConstBool   <$> parseBool)

prefixOp :: Char -> UnaryOp -> Operator Parser Expr
prefixOp symbol op = Prefix (char symbol *> space $> UnaryOpExpr op)

infixLOp :: Char -> BinaryOp -> Operator Parser Expr
infixLOp symbol op = InfixL (char symbol *> space $> BinOpExpr op)

exprOpTable :: [[Operator Parser Expr]]
exprOpTable = [ [ prefixOp '-' Negate
                ]
              , [ infixLOp '*' Mul
                , infixLOp '/' Div
                ]
              , [ infixLOp '+' Add
                , infixLOp '-' Sub
                ]
              ]

parseExprTerm :: Parser Expr 
parseExprTerm = choice
    [ char '(' *> parseExpr <* char ')' <* space
    , ConstExpr <$> parseConstant
    ]

parseExpr :: Parser Expr
parseExpr = makeExprParser parseExprTerm exprOpTable

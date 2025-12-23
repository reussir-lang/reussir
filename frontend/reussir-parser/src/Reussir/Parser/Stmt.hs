module Reussir.Parser.Stmt where

import Data.Maybe

import Reussir.Parser.Expr
import Reussir.Parser.Lexer

import Reussir.Parser.Types
import Reussir.Parser.Types.Expr
import Reussir.Parser.Types.Lexer (Identifier)
import Reussir.Parser.Types.Stmt

parseVis :: Parser Visibility
parseVis =
    optional (string "pub" *> space) >>= \case
        Just () -> return Public
        Nothing -> return Private

parseTypedParam :: Parser (Identifier, Typename)
parseTypedParam = do
    name <- parseIdentifier <* char ':' <* space
    ty <- parseTypename

    return (name, ty)

parseStructDec :: Parser Stmt
parseStructDec = do
    vis <- parseVis
    name <- string "struct" *> space *> parseIdentifier
    types <- openParen *> parseTypename `sepBy` comma <* closeParen

    return (Struct vis name types)

parseFuncDef :: Parser Stmt
parseFuncDef = do
    vis <- parseVis
    name <- string "fn" *> space *> parseIdentifier
    tyargs <- optional $ openAngle *> parseIdentifier `sepBy` comma <* closeAngle
    args <- openParen *> optional (parseTypedParam `sepBy` comma)
    ret <- closeParen *> optional (string "->" *> space *> parseTypename)
    body <- parseBody

    return (Function vis name (fromMaybe [] tyargs) (fromMaybe [] args) ret body)

parseEnumConstructor :: Parser (Identifier, [Typename])
parseEnumConstructor = do
    name <- parseIdentifier
    tys <- optional $ openParen *> parseTypename `sepBy` comma <* closeParen
    return (name, fromMaybe [] tys)

parseEnumDec :: Parser Stmt
parseEnumDec = do
    vis <- parseVis
    name <- string "enum" *> space *> parseIdentifier
    tyvars <- openAngle *> parseIdentifier `sepBy` comma <* closeAngle
    body <- openBody *> parseEnumConstructor `sepBy` comma <* closeBody

    return (Enum vis name tyvars body)

parseStmt :: Parser Stmt
parseStmt =
    try parseFuncDef
        <|> try parseStructDec
        <|> try parseEnumDec

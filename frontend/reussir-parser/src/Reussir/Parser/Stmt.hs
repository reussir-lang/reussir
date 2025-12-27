{-# LANGUAGE OverloadedStrings #-}

module Reussir.Parser.Stmt where

import Data.Maybe

import Reussir.Parser.Expr
import Reussir.Parser.Lexer

import Reussir.Parser.Type (parseType)
import Reussir.Parser.Types
import Reussir.Parser.Types.Lexer (Identifier)
import Reussir.Parser.Types.Stmt
import Reussir.Parser.Types.Type (Type)

parseVis :: Parser Visibility
parseVis =
    optional (string "pub" *> space) >>= \case
        Just () -> return Public
        Nothing -> return Private

parseTypedParam :: Parser (Identifier, Type)
parseTypedParam = do
    name <- parseIdentifier <* char ':' <* space
    ty <- parseType

    return (name, ty)

parseCapability :: Parser Capability
parseCapability =
    optional (char '[' *> space *> parseCapKeyword <* char ']' <* space) >>= \case
        Nothing -> return Unspecified
        Just c -> return c

parseCapKeyword :: Parser Capability
parseCapKeyword =
    choice
        [ string "shared" >> return Shared
        , string "value" >> return Value
        , string "flex" >> return Flex
        , string "rigid" >> return Rigid
        , string "field" >> return Field
        ]
        <* space

parseStructDec :: Parser Stmt
parseStructDec = do
    vis <- parseVis
    _ <- string "struct" *> space
    cap <- parseCapability
    name <- parseIdentifier
    fields <- try parseNamedFields <|> parseUnnamedFields
    return $ RecordStmt $ Record name [] fields StructKind vis cap

parseUnnamedFields :: Parser RecordFields
parseUnnamedFields = do
    types <- openParen *> parseFieldType `sepBy` comma <* closeParen
    return $ Unnamed types
  where
    parseFieldType = do
        cap <- parseCapability
        ty <- parseType
        return (ty, cap)

parseNamedFields :: Parser RecordFields
parseNamedFields = do
    fields <- openBody *> parseNamedField `sepBy` comma <* closeBody
    return $ Named fields

parseNamedField :: Parser (Identifier, Type, Capability)
parseNamedField = do
    name <- parseIdentifier <* colon
    cap <- parseCapability
    ty <- parseType
    return (name, ty, cap)

parseFuncDef :: Parser Stmt
parseFuncDef = do
    vis <- parseVis
    name <- string "fn" *> space *> parseIdentifier
    tyargs <- optional $ openAngle *> parseIdentifier `sepBy` comma <* closeAngle
    args <- openParen *> optional (parseTypedParam `sepBy` comma)
    ret <- closeParen *> optional (string "->" *> space *> parseType)
    body <- parseBody

    return (Function vis name (fromMaybe [] tyargs) (fromMaybe [] args) ret body)

parseEnumConstructor :: Parser (Identifier, [Type])
parseEnumConstructor = do
    name <- parseIdentifier
    tys <- optional $ openParen *> parseType `sepBy` comma <* closeParen
    return (name, fromMaybe [] tys)

parseEnumDec :: Parser Stmt
parseEnumDec = do
    vis <- parseVis
    name <- string "enum" *> space *> parseIdentifier
    tyvars <- openAngle *> parseIdentifier `sepBy` comma <* closeAngle
    body <- openBody *> parseEnumConstructor `sepBy` comma <* closeBody

    let fields = Variants body
    return $ RecordStmt $ Record name tyvars fields EnumKind vis Unspecified

parseStmt :: Parser Stmt
parseStmt = SpannedStmt <$> withSpan parseStmtInner

parseStmtInner :: Parser Stmt
parseStmtInner =
    try parseFuncDef
        <|> try parseStructDec
        <|> try parseEnumDec

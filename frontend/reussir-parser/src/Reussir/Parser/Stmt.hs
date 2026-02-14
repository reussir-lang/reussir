{-# LANGUAGE OverloadedStrings #-}

module Reussir.Parser.Stmt where

import Data.Maybe

import Data.Vector.Strict qualified as V

import Reussir.Parser.Expr
import Reussir.Parser.Lexer
import Reussir.Parser.Type (parseType)
import Reussir.Parser.Types hiding (space)
import Reussir.Parser.Types.Capability (Capability (Shared))
import Reussir.Parser.Types.Lexer (Identifier (..), Path, WithSpan)
import Reussir.Parser.Types.Stmt
import Reussir.Parser.Types.Type (Type)

parseVis :: Parser Visibility
parseVis =
    optional (string "pub" *> space) >>= \case
        Just () -> return Public
        Nothing -> return Private

parseFieldFlag :: Parser Bool
parseFieldFlag =
    option
        False
        (True <$ (char '[' *> space *> string "field" <* space <* char ']' <* space))

parseFlexFlag :: Parser Bool
parseFlexFlag =
    option
        False
        (True <$ (char '[' *> space *> string "flex" <* space <* char ']' <* space))

parseTypedParam :: Parser (Identifier, Type, Bool)
parseTypedParam = do
    name <- parseIdentifier <* char ':' <* space
    flx <- parseFlexFlag
    ty <- parseType

    return (name, ty, flx)

parseStructDec :: Parser Stmt
parseStructDec = parseVis >>= parseStructDecRest

parseStructDecRest :: Visibility -> Parser Stmt
parseStructDecRest vis = do
    _ <- string "struct" *> space
    cap <- parseCapability Shared
    name <- parseIdentifier
    tyvars <- optional $ openAngle *> parseGenericParam `sepBy` comma <* closeAngle
    fields <- try parseNamedFields <|> parseUnnamedFields
    return $
        RecordStmt $
            Record name (fromMaybe [] tyvars) fields StructKind vis cap

parseUnnamedFields :: Parser RecordFields
parseUnnamedFields = do
    types <- openParen *> parseFieldType `sepBy` comma <* closeParen
    return $ Unnamed (V.fromList types)
  where
    parseFieldType = withSpan $ do
        fld <- parseFieldFlag
        ty <- parseType
        return (ty, fld)

parseNamedFields :: Parser RecordFields
parseNamedFields = do
    fields <- openBody *> parseNamedField `sepBy` comma <* closeBody
    return $ Named (V.fromList fields)

parseNamedField :: Parser (WithSpan (Identifier, Type, Bool))
parseNamedField = withSpan $ do
    name <- parseIdentifier <* colon
    fld <- parseFieldFlag
    ty <- parseType
    return (name, ty, fld)

parseFuncDef :: Parser Stmt
parseFuncDef = parseVis >>= parseFuncDefRest

parseGenericParam :: Parser (Identifier, [Path])
parseGenericParam = do
    name <- parseIdentifier
    bounds <- optional (char ':' *> space *> parsePath `sepBy` (char '+' *> space))
    return (name, fromMaybe [] bounds)

parseFuncDefRest :: Visibility -> Parser Stmt
parseFuncDefRest vis = do
    isRegional <- isJust <$> optional (string "regional" *> space)
    name <- string "fn" *> space *> parseIdentifier
    tyargs <- optional $ openAngle *> parseGenericParam `sepBy` comma <* closeAngle
    args <- openParen *> optional (parseTypedParam `sepBy` comma)
    ret <- closeParen *> optional (string "->" *> space *> parseRetType)
    body <- (Just <$> parseBody) <|> (Nothing <$ semicolon)

    return
        ( FunctionStmt $
            Function vis name (fromMaybe [] tyargs) (fromMaybe [] args) ret isRegional body
        )
  where
    parseRetType = do
        flx <- parseFlexFlag
        ty <- parseType
        return (ty, flx)

parseEnumConstructor :: Parser (Identifier, V.Vector Type)
parseEnumConstructor = do
    name <- parseIdentifier
    tys <-
        fmap V.fromList
            <$> optional (openParen *> parseType `sepBy` comma <* closeParen)
    return (name, fromMaybe V.empty tys)

parseEnumDec :: Parser Stmt
parseEnumDec = parseVis >>= parseEnumDecRest

parseEnumDecRest :: Visibility -> Parser Stmt
parseEnumDecRest vis = do
    _ <- string "enum" *> space
    cap <- parseCapability Shared
    name <- parseIdentifier
    tyvars <- optional $ openAngle *> parseGenericParam `sepBy` comma <* closeAngle
    body <- openBody *> (withSpan parseEnumConstructor) `sepBy` comma <* closeBody

    let fields = Variants $ V.fromList body
    return $ RecordStmt $ Record name (fromMaybe [] tyvars) fields EnumKind vis cap

parseExternTrampoline :: Parser Stmt
parseExternTrampoline = do
    _ <- string "extern" *> space
    abi <- parseString <* space
    _ <- string "trampoline" *> space
    sym <- parseString <* space
    _ <- char '=' *> space
    func <- parsePath <* semicolon
    funcTyArgs <- optional (openAngle *> parseType `sepBy` comma <* closeAngle)
    return $ ExternTrampolineStmt (Identifier sym) abi func $ fromMaybe [] funcTyArgs

parseStmt :: Parser Stmt
parseStmt = SpannedStmt <$> withSpan parseStmtInner

parseStmtInner :: Parser Stmt
parseStmtInner = do
    vis <- parseVis
    choice
        [ parseFuncDefRest vis <?> "function definition"
        , parseStructDecRest vis <?> "struct declaration"
        , parseEnumDecRest vis <?> "enum declaration"
        , parseExternTrampoline <?> "extern trampoline"
        ]

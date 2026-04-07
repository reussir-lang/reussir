{-# LANGUAGE OverloadedStrings #-}

module Reussir.Parser.Stmt where

import Control.Monad (join)
import Data.Maybe

import Data.Vector.Strict qualified as V

import Data.Text qualified as T

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

-- | Parse the legacy trampoline syntax, desugaring to ExternFFIStmt.
-- @extern \"C\" trampoline \"bar\" = foo\<i32\>;@  →  @extern \"C\" export fn bar = foo\<i32\>;@
parseExternTrampoline :: Parser Stmt
parseExternTrampoline = do
    _ <- string "extern" *> space
    abi <- parseString <* space
    _ <- string "trampoline" *> space
    sym <- parseString <* space
    _ <- char '=' *> space
    func <- parsePath <* space
    funcTyArgs <- optional (openAngle *> parseType `sepBy` comma <* closeAngle) <* semicolon
    return $ ExternFFIStmt
        { efsABI = abi
        , efsDirection = FFIExport
        , efsName = Identifier sym
        , efsGenerics = []
        , efsParams = []
        , efsReturnType = Nothing
        , efsBody = FFIAlias func (fromMaybe [] funcTyArgs)
        }

-- | Parse a triple-backtick quoted region.
-- Consumes everything between @\`\`\`@ delimiters as raw text.
parseQuotedTemplate :: Parser T.Text
parseQuotedTemplate = do
    _ <- string "```"
    -- Only skip the rest of the opening line (not comments inside the template)
    _ <- takeWhileP Nothing (\c -> c == ' ' || c == '\t')
    _ <- optional (char '\n')
    content <- manyTill anySingle (string "```")
    space
    return $ T.strip $ T.pack content

-- | Parse a typed parameter for FFI declarations (no flex flag).
parseFFIParam :: Parser (Identifier, Type)
parseFFIParam = do
    name <- parseIdentifier <* char ':' <* space
    ty <- parseType
    return (name, ty)

-- | Parse the body of an FFI declaration.
-- Either an alias (@= path\<args\>@), a quoted template, or just a semicolon.
parseFFIBody :: FFIDirection -> Parser FFIBody
parseFFIBody FFIExport = do
    -- Export must have alias: = path<args>;
    _ <- char '=' *> space
    func <- parsePath <* space
    funcTyArgs <- optional (openAngle *> parseType `sepBy` comma <* closeAngle)
    semicolon
    return $ FFIAlias func (fromMaybe [] funcTyArgs)
parseFFIBody FFIImport = do
    -- Import: either semicolon (simple extern) or { ```...``` } (template)
    choice
        [ FFIExtern <$ semicolon
        , do
            openBody
            template <- parseQuotedTemplate
            closeBody
            return $ FFITemplate template
        ]

-- | Parse a new-style FFI declaration.
--
-- @extern \"C\" import fn name\<T\>(params) -> rettype { \`\`\`template\`\`\` }@
-- @extern \"C\" export fn name = target\<args\>;@
parseExternFFI :: Parser Stmt
parseExternFFI = do
    _ <- string "extern" *> space
    abi <- parseString <* space
    direction <- parseDirection <* space
    _ <- string "fn" *> space
    name <- parseIdentifier
    tyargs <- optional $ openAngle *> parseGenericParam `sepBy` comma <* closeAngle
    params <- case direction of
        FFIImport -> do
            ps <- openParen *> optional (parseFFIParam `sepBy` comma)
            closeParen
            return $ fromMaybe [] ps
        FFIExport -> do
            -- Export may optionally have params for documentation, or omit them
            ps <- optional (openParen *> optional (parseFFIParam `sepBy` comma) <* closeParen)
            return $ fromMaybe [] (join ps)
    ret <- optional (string "->" *> space *> parseType)
    body <- parseFFIBody direction
    return $ ExternFFIStmt
        { efsABI = abi
        , efsDirection = direction
        , efsName = name
        , efsGenerics = fromMaybe [] tyargs
        , efsParams = params
        , efsReturnType = ret
        , efsBody = body
        }
  where
    parseDirection :: Parser FFIDirection
    parseDirection =
        (FFIImport <$ string "import")
            <|> (FFIExport <$ string "export")

parseStmt :: Parser Stmt
parseStmt = SpannedStmt <$> withSpan parseStmtInner

parseExternStruct :: Parser Stmt
parseExternStruct = do
    _ <- string "extern" *> space
    _ <- string "struct" *> space
    name <- parseIdentifier
    tyargs <- optional $ openAngle *> parseGenericParam `sepBy` comma <* closeAngle
    _ <- char '=' *> space
    foreignType <- parseString <* space
    _ <- char ';' *> space
    return $ ExternStructStmt
        { essName = name
        , essGenerics = fromMaybe [] tyargs
        , essForeignType = foreignType
        }

parseStmtInner :: Parser Stmt
parseStmtInner = do
    vis <- parseVis
    choice
        [ parseFuncDefRest vis <?> "function definition"
        , parseStructDecRest vis <?> "struct declaration"
        , parseEnumDecRest vis <?> "enum declaration"
        , parseModDeclRest vis <?> "module declaration"
        , try parseExternTrampoline <?> "extern trampoline"
        , try parseExternStruct <?> "extern struct declaration"
        , parseExternFFI <?> "extern FFI declaration"
        ]

parseModDeclRest :: Visibility -> Parser Stmt
parseModDeclRest vis = do
    _ <- string "mod" *> space
    name <- parseIdentifier <* semicolon
    return $ ModStmt vis name

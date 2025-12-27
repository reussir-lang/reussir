{-# LANGUAGE OverloadedStrings #-}

{- | Module for lexical analysis in the Reussir parser.
Provides parsers for tokens, literals, and basic syntactic elements.
-}
module Reussir.Parser.Lexer (
    -- * Delimiters and operators
    colon,
    semicolon,
    doubleColon,
    openParen,
    closeParen,
    comma,
    openBody,
    closeBody,
    openAngle,
    closeAngle,
    rightArrow,
    dot,

    -- * Identifiers and paths
    parseIdentifier,
    parsePath,

    -- * Literals
    parseInt,
    parseDouble,
    parseString,
    parseBool,

    -- * Capabilities
    parseCapability,

    -- * Type suffixes
    parseIntSuffix,
    parseFloatSuffix,

    -- * Utilities
    withSpan,
) where

import Data.Functor (($>))
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Reussir.Parser.Types
import Reussir.Parser.Types.Capability (Capability (..))
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))
import Text.Megaparsec.Char.Lexer (charLiteral)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Unicode.Char qualified as U

-- | Parse a colon ':' followed by whitespace.
colon :: Parser ()
colon = char ':' *> space

-- | Parse a semicolon ';' followed by whitespace.
semicolon :: Parser ()
semicolon = char ';' *> space

-- | Parse a double colon '::' followed by whitespace.
doubleColon :: Parser ()
doubleColon = string "::" *> space

-- | Parse an opening parenthesis '(' followed by whitespace.
openParen :: Parser ()
openParen = char '(' *> space

-- | Parse a closing parenthesis ')' followed by whitespace.
closeParen :: Parser ()
closeParen = char ')' *> space

-- | Parse a comma ',' followed by whitespace.
comma :: Parser ()
comma = char ',' *> space

-- | Parse an opening brace '{' followed by whitespace.
openBody :: Parser ()
openBody = char '{' *> space

-- | Parse a closing brace '}' followed by whitespace.
closeBody :: Parser ()
closeBody = char '}' *> space

-- | Parse an opening angle bracket '<' followed by whitespace.
openAngle :: Parser ()
openAngle = char '<' *> space

-- | Parse a closing angle bracket '>' followed by whitespace.
closeAngle :: Parser ()
closeAngle = char '>' *> space

-- | Parse a right arrow '->' followed by whitespace.
rightArrow :: Parser ()
rightArrow = string "->" *> space

-- | Parse a dot '.' followed by whitespace.
dot :: Parser ()
dot = char '.' *> space

{- | Parse an identifier according to Unicode XID (eXtended IDentifier) rules.
An identifier must start with a character matching 'isXIDStart' and
continue with characters matching 'isXIDContinue'.
-}
parseIdentifier :: Parser Identifier
parseIdentifier = do
    first <- satisfy U.isXIDStart
    rest <- many (satisfy U.isXIDContinue) <* space
    pure $ Identifier $ T.pack (first : rest)

{- | Parse an integer type suffix (u8, u16, u32, u64).
Consumes the suffix followed by whitespace.
-}
parseIntSuffix :: Parser ()
parseIntSuffix = choice [string $ T.pack ('u' : show @Int s) | s <- [8, 16, 32, 64]] *> space

{- | Parse a floating-point type suffix (f16, f32, f64).
Consumes the suffix followed by whitespace.
-}
parseFloatSuffix :: Parser ()
parseFloatSuffix = choice [string $ T.pack ('f' : show @Int s) | s <- [16, 32, 64]] *> space

{- | Parse an integer literal with optional type suffix.
Does not accept floating-point notation (rejects '.' and 'e'/'E').
-}
parseInt :: Parser Int
parseInt = try $ do
    n <- some digitChar
    notFollowedBy (char '.' <|> char 'e' <|> char 'E')
    _ <- optional parseIntSuffix
    space
    return (read n)

{- | Parse a floating-point literal in scientific notation with optional type suffix.
Returns a 'Scientific' value for arbitrary precision.
-}
parseDouble :: Parser Scientific
parseDouble = Lexer.scientific <* optional parseFloatSuffix <* space

{- | Parse a string literal enclosed in double quotes.
Supports escape sequences via 'charLiteral'.
-}
parseString :: Parser T.Text
parseString =
    T.pack <$> (char '"' *> manyTill charLiteral (char '"') <* space)

-- | Parse a boolean literal ('true' or 'false').
parseBool :: Parser Bool
parseBool =
    choice
        [ string "true" $> True
        , string "false" $> False
        ]
        <* space

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

{- | Wrap a parser with source span information.
Records the start and end byte offsets of the parsed value.
-}
withSpan :: Parser a -> Parser (WithSpan a)
withSpan p = do
    start <- fmap fromIntegral getOffset
    val <- p
    end <- fmap fromIntegral getOffset
    return $ WithSpan val start end

{- | Parse a namespace-qualified path separated by '::'.
Examples: 'foo', 'std::io', 'std::io::File'
The path segments are stored in order, with the basename as the final component.
-}
parsePath :: Parser Path
parsePath = do
    ids <- parseIdentifier `sepBy1` doubleColon
    let segments = init ids
    let basename = last ids
    return (Path basename segments)

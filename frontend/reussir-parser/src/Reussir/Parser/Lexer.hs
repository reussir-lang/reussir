module Reussir.Parser.Lexer where

import Data.Functor (($>))
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Reussir.Parser.Types
import Reussir.Parser.Types.Lexer (Identifier (..), WithSpan (..))
import Text.Megaparsec.Char.Lexer (charLiteral)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Unicode.Char qualified as U

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

parseIdentifier :: Parser Identifier
parseIdentifier = do
    first <- satisfy U.isXIDStart
    rest <- many (satisfy U.isXIDContinue) <* space
    pure $ Identifier $ T.pack (first : rest)

parseIntSuffix :: Parser ()
parseIntSuffix = choice [string ('u' : show @Int s) | s <- [8, 16, 32, 64]] *> space

parseFloatSuffix :: Parser ()
parseFloatSuffix = choice [string ('f' : show @Int s) | s <- [16, 32, 64]] *> space

parseInt :: Parser Int
parseInt = try $ do
    n <- some digitChar
    notFollowedBy (char '.' <|> char 'e' <|> char 'E')
    _ <- optional parseIntSuffix
    space
    return (read n)

parseDouble :: Parser Scientific
parseDouble = Lexer.scientific <* optional parseFloatSuffix <* space

parseString :: Parser T.Text
parseString =
    T.pack <$> (char '"' *> manyTill charLiteral (char '"') <* space)

parseBool :: Parser Bool
parseBool =
    choice
        [ string "true" $> True
        , string "false" $> False
        ]
        <* space

withSpan :: Parser a -> Parser (WithSpan a)
withSpan p = do
    start <- fmap fromIntegral getOffset
    val <- p
    end <- fmap fromIntegral getOffset
    return $ WithSpan val start end

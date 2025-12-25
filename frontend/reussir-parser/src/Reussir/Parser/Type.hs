-- | Module for type parsing functionality.
module Reussir.Parser.Type where

import Reussir.Parser.Types
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))
import Reussir.Parser.Types.Type (Type)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Text.Megaparsec.Char.Lexer qualified as Lexer

parseType :: Parser Type
parseType = undefined

parseIntegralType :: Parser Type
parseIntegralType = undefined

parseFPType :: Parser Type
parseFPType = undefined

parseTyprExpr :: Parser Type
parseTyprExpr = undefined

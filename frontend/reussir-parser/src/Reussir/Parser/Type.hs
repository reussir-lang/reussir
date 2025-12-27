{-# LANGUAGE OverloadedStrings #-}

-- | Module for type parsing functionality.
module Reussir.Parser.Type where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Reussir.Parser.Lexer (closeAngle, comma, openAngle, parsePath, withSpan)
import Reussir.Parser.Types
import Reussir.Parser.Types.Type (FloatingPointType (..), IntegralType (..), Type (..))
import Unicode.Char qualified as U

parseType :: Parser Type
parseType = TypeSpanned <$> withSpan parseArrowType

parseTypeAtom :: Parser Type
parseTypeAtom =
    choice
        [ parseIntegralType <?> "integral type"
        , parseFPType <?> "floating point type"
        , (string "bool" *> notFollowedBy (satisfy U.isXIDContinue) *> space $> TypeBool) <?> "bool"
        , (string "str" *> notFollowedBy (satisfy U.isXIDContinue) *> space $> TypeStr) <?> "str"
        , (string "unit" *> notFollowedBy (satisfy U.isXIDContinue) *> space $> TypeUnit) <?> "unit"
        , parseTypeExpr <?> "type expression"
        , (between (string "(" *> space) (string ")" *> space) parseType) <?> "parenthesized type"
        ]

parseIntegralType :: Parser Type
parseIntegralType = try $ do
    c <- oneOf ['i', 'u']
    width <- choice [string "8" $> 8, string "16" $> 16, string "32" $> 32, string "64" $> 64]
    notFollowedBy (satisfy U.isXIDContinue)
    space
    let it = if c == 'i' then Signed width else Unsigned width
    return (TypeIntegral it)

parseFPType :: Parser Type
parseFPType = try $ do
    t <-
        choice
            [ string "f16" $> IEEEFloat 16
            , string "f32" $> IEEEFloat 32
            , string "f64" $> IEEEFloat 64
            , string "bfloat16" $> BFloat16
            , string "float8" $> Float8
            ]
    notFollowedBy (satisfy U.isXIDContinue)
    space
    return (TypeFP t)

parseArrowType :: Parser Type
parseArrowType = makeExprParser parseTypeAtom table
  where
    table = [[InfixR (string "->" *> space $> TypeArrow)]]

parseTypeExpr :: Parser Type
parseTypeExpr = do
    path <- parsePath
    args <- optional (openAngle *> parseType `sepBy1` comma <* closeAngle)
    return $ TypeExpr path (fromMaybe [] args)

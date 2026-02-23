{-# LANGUAGE OverloadedStrings #-}

-- | Module for type parsing functionality.
module Reussir.Parser.Type where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor (($>))
import Data.List (singleton)
import Data.Maybe (fromMaybe)

import Unicode.Char qualified as U

import Reussir.Parser.Lexer (
    closeAngle,
    comma,
    openAngle,
    parsePath,
    space,
    withSpan,
 )
import Reussir.Parser.Types hiding (space)
import Reussir.Parser.Types.Type (
    FloatingPointType (..),
    IntegralType (..),
    Type (..),
 )

parseType :: Parser Type
parseType = TypeSpanned <$> withSpan parseArrowType

parseTypeAtom :: Parser Type
parseTypeAtom =
    choice
        [ parseIntegralType <?> "integral type"
        , parseFPType <?> "floating point type"
        , (string "bool" *> notFollowedBy (satisfy U.isXIDContinue) *> space $> TypeBool)
            <?> "bool"
        , (string "str" *> notFollowedBy (satisfy U.isXIDContinue) *> space $> TypeStr)
            <?> "str"
        , (string "unit" *> notFollowedBy (satisfy U.isXIDContinue) *> space $> TypeUnit)
            <?> "unit"
        , parseTypeExpr <?> "type expression"
        ]

parseIntegralType :: Parser Type
parseIntegralType = try $ do
    c <- oneOf ['i', 'u']
    width <-
        choice
            [string "8" $> 8, string "16" $> 16, string "32" $> 32, string "64" $> 64]
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

parseTypeList :: Parser [Type]
parseTypeList =
    try $
        between (char '(' *> space) (char ')' *> space) $
            sepBy1 parseType comma

data MaybeArrow
    = ArrowList [Type]
    | ArrowSingle Type
    | ArrowCons MaybeArrow MaybeArrow

parseArrowAtom :: Parser MaybeArrow
parseArrowAtom = choice [arrowList <$> parseTypeList, ArrowSingle <$> parseTypeAtom]
  where
    arrowList [] = ArrowSingle TypeUnit
    arrowList [x] = ArrowSingle x
    arrowList xs = ArrowList xs

parseArrowSegments :: Parser MaybeArrow
parseArrowSegments = makeExprParser parseArrowAtom table
  where
    table = [[InfixR (string "->" *> space $> ArrowCons)]]

parseArrowType :: Parser Type
parseArrowType = do
    parsed <- parseArrowSegments
    case maybeArrowToMaybeType parsed of
        Just ty -> pure ty
        Nothing ->
            fail
                "invalid arrow type: expected a single return type at the end of the arrow chain"
  where
    maybeArrowToMaybeType :: MaybeArrow -> Maybe Type
    maybeArrowToMaybeType (ArrowSingle ty) = Just ty
    maybeArrowToMaybeType (ArrowList _) = Nothing
    maybeArrowToMaybeType (ArrowCons lhs rhs) = do
        lhsArgs <- maybeArrowToArgs lhs
        rhsTy <- maybeArrowToMaybeType rhs
        pure $ TypeArrow lhsArgs rhsTy

    maybeArrowToArgs :: MaybeArrow -> Maybe [Type]
    maybeArrowToArgs (ArrowSingle ty) = Just (singleton ty)
    maybeArrowToArgs (ArrowList tys) = Just tys
    maybeArrowToArgs arrowNode = singleton <$> maybeArrowToMaybeType arrowNode

parseTypeExpr :: Parser Type
parseTypeExpr = do
    path <- parsePath
    args <- optional (openAngle *> parseType `sepBy1` comma <* closeAngle)
    return $ TypeExpr path (fromMaybe [] args)

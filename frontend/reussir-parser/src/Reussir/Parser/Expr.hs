{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Reussir.Parser.Expr where

import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Maybe
import Data.Vector (Vector)

import Data.Char qualified as C
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Strict qualified as SV

import Reussir.Parser.Lexer
import Reussir.Parser.Type (parseType)
import Reussir.Parser.Types hiding (space)
import Reussir.Parser.Types.Expr
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..))
import Reussir.Parser.Types.Type (Type)

parseBody :: Parser Expr
parseBody = parseExprSeq

parseExprSeq :: Parser Expr
parseExprSeq = ExprSeq <$> (openBody *> parseExpr `sepEndBy` semicolon <* closeBody)

parsePattern :: Parser Pattern
parsePattern = do
    kind <- parsePatternKind
    guard <- optional (try (keyword "if") *> parseExpr)
    return (Pattern kind guard)
  where
    keyword k = string k *> notFollowedBy (satisfy C.isAlphaNum) *> space

parsePatternKind :: Parser PatternKind
parsePatternKind =
    choice
        [ WildcardPat <$ (char '_' *> space)
        , ConstPat <$> parseConstant
        , parsePathOrBind
        ]

parsePathOrBind :: Parser PatternKind
parsePathOrBind = do
    p <- parsePath
    choice
        [ do
            (args, ell) <- parseCtorPatArgs (char '{') (char '}') True
            return (CtorPat p args ell True)
        , do
            (args, ell) <- parseCtorPatArgs (char '(') (char ')') False
            return (CtorPat p args ell False)
        , return $ case p of
            Path _ (_ : _) -> CtorPat p V.empty False False
            Path (Identifier t) [] ->
                if C.isUpper (T.head t)
                    then CtorPat p V.empty False False
                    else BindPat (Identifier t)
        ]

parseCtorPatArgs ::
    Parser Char -> Parser Char -> Bool -> Parser (Vector PatternCtorArg, Bool)
parseCtorPatArgs open close isNamed = do
    _ <- open *> space
    choice
        [ do
            _ <- string ".." *> space
            _ <- close *> space
            return (V.empty, True)
        , do
            (args, ell) <- parseArgsAndEllipsis isNamed
            _ <- close *> space
            return (V.fromList args, ell)
        , do
            _ <- close *> space
            return (V.empty, False)
        ]

parseArgsAndEllipsis :: Bool -> Parser ([PatternCtorArg], Bool)
parseArgsAndEllipsis isNamed = do
    arg <- parseArg isNamed
    choice
        [ do
            _ <- comma
            choice
                [ do
                    _ <- string ".." *> space
                    return ([arg], True)
                , do
                    (rest, ell) <- parseArgsAndEllipsis isNamed
                    return (arg : rest, ell)
                , return ([arg], False)
                ]
        , return ([arg], False)
        ]

parseArg :: Bool -> Parser PatternCtorArg
parseArg True = do
    -- Named: identifier (: pattern)?
    id' <- parseIdentifier
    mk <- optional (colon *> parsePatternKind)
    case mk of
        Just k -> return $ PatternCtorArg (Just id') k
        Nothing -> return $ PatternCtorArg (Just id') (BindPat id') -- { x } -> x: x (BindPat)
parseArg False = do
    -- Positional: pattern
    k <- parsePatternKind
    return $ PatternCtorArg Nothing k

parseIf :: Parser Expr
parseIf = do
    cond <- string "if" *> space *> parseExpr
    iftrue <- parseBody
    iffalse <- string "else" *> space *> parseBody

    return (If cond iftrue iffalse)

parseLetIn :: Parser Expr
parseLetIn = do
    name <- string "let" *> space *> withSpan parseIdentifier
    ty <- optional (colon *> parseTypeWithFlex)
    value <- char '=' *> space *> parseExpr
    return (Let name ty value)
  where
    parseTypeWithFlex = do
        flexFlag <-
            optional (char '[' *> space *> string "flex" <* space <* char ']' <* space)
        ty <- parseType
        return (ty, isJust flexFlag)

assignOp :: Operator Parser Expr
assignOp = InfixN $ do
    access <- space *> parseArrowAccess <* string ":=" <* space
    return $ \e1 e2 -> Assign e1 access e2

parseLambda :: Parser Expr
parseLambda = do
    _ <- char '|' *> space
    args <- parseLambdaArg `sepBy` comma
    _ <- char '|' *> space
    body <- parseExpr

    return (Lambda (LambdaExpr args body))
  where
    parseLambdaArg = do
        name <- parseIdentifier
        ty <- optional (colon *> parseType)
        return (name, ty)

parseMatchCase :: Parser (Pattern, Expr)
parseMatchCase = do
    pat <- parsePattern
    expr <- string "=>" *> space *> parseExpr
    return (pat, expr)

parseMatch :: Parser Expr
parseMatch = do
    expr <- string "match" *> space *> parseExprWithOpts False
    body <- openBody *> parseMatchCase `sepBy` comma <* closeBody
    return (Match expr (SV.fromList body))

parseConstant :: Parser Constant
parseConstant =
    try (ConstInt <$> parseInt)
        <|> (ConstDouble <$> parseDouble)
        <|> (ConstString <$> parseString)
        <|> (ConstBool <$> parseBool)

parseTypeArg :: Parser (Maybe Type)
parseTypeArg = (char '_' *> space $> Nothing) <|> (Just <$> parseType)

parseCtorArg :: Parser (Maybe Identifier, Expr)
parseCtorArg =
    try
        ( do
            id' <- parseIdentifier
            colon
            e <- parseExpr
            return (Just id', e)
        )
        <|> ( do
                e <- parseExpr
                return (Nothing, e)
            )

parseCtorArgs :: Parser [(Maybe Identifier, Expr)]
parseCtorArgs =
    choice
        [ openBody *> parseCtorArg `sepBy` comma <* closeBody
        , do
            exprs <- openParen *> parseExpr `sepBy` comma <* closeParen
            return $ map (Nothing,) exprs
        , pure []
        ]

parseRegionalExpr :: Parser Expr
parseRegionalExpr = do
    _ <- try (string "regional" *> space *> lookAhead (char '{'))
    body <- parseBody
    return (RegionalExpr body)

parsePathBasedExpr :: Bool -> Parser Expr
parsePathBasedExpr allowStruct = do
    path <- parsePath
    tyArgs <- optional (try (openAngle *> parseTypeArg `sepBy` comma <* closeAngle))
    lookAheadChar <- optional (lookAhead anySingle)
    case lookAheadChar of
        Just '{' | allowStruct -> do
            args <- openBody *> parseCtorArg `sepBy` comma <* closeBody
            return $ CtorCallExpr $ CtorCall path (fromMaybe [] tyArgs) args
        Just '(' -> do
            args <- openParen *> parseExpr `sepBy` comma <* closeParen
            return $ FuncCallExpr $ FuncCall path (fromMaybe [] tyArgs) args
        _ -> do
            case tyArgs of
                Just ts -> return $ CtorCallExpr $ CtorCall path ts []
                Nothing -> return $ Var path

prefixOp :: T.Text -> UnaryOp -> Operator Parser Expr
prefixOp symbol op = Prefix (string symbol *> space $> UnaryOpExpr op)

infixLOp :: T.Text -> BinaryOp -> Operator Parser Expr
infixLOp symbol op = case symbol of
    "-" ->
        -- need to specially handle the ambiguity with arrow assignment
        let sub = try $ string symbol *> notFollowedBy (char '>')
         in InfixL (sub *> space $> BinOpExpr op)
    _ -> InfixL (string symbol *> space $> BinOpExpr op)

infixNOp :: T.Text -> BinaryOp -> Operator Parser Expr
infixNOp symbol op = InfixN (string symbol *> space $> BinOpExpr op)

castOp :: Operator Parser Expr
castOp = Postfix $ do
    ty <- string "as" *> space *> parseType
    return (Cast ty)

parseAccess :: Parser Access
parseAccess =
    dot
        *> ( (Named <$> parseIdentifier)
                <|> (Unnamed . read <$> (some digitChar <* space))
           )

parseArrowAccess :: Parser Access
parseArrowAccess =
    arrow
        *> ( (Named <$> parseIdentifier)
                <|> (Unnamed . read <$> (some digitChar <* space))
           )

accessOp :: Operator Parser Expr
accessOp = Postfix $ do
    access <- some parseAccess
    return $ flip AccessChain (SV.fromList access)

exprOpTable :: [[Operator Parser Expr]]
exprOpTable =
    [
        [ prefixOp "-" Negate
        , prefixOp "!" Not
        , castOp
        , accessOp
        ]
    ,
        [ infixLOp "*" Mul
        , infixLOp "/" Div
        , infixLOp "%" Mod
        ]
    ,
        [ infixLOp "+" Add
        , infixLOp "-" Sub
        ]
    ,
        [ infixNOp ">=" Gte
        , infixNOp "<=" Lte
        , infixNOp ">" Gt
        , infixNOp "<" Lt
        , infixNOp "==" Equ
        , infixNOp "!=" Neq
        ]
    ,
        [ infixLOp "&&" And
        ]
    ,
        [ infixLOp "||" Or
        ]
    ,
        [ assignOp
        ]
    ]

parseExprTerm :: Bool -> Parser Expr
parseExprTerm allowStruct =
    choice
        [ (char '(' *> space *> parseExpr <* space <* char ')' <* space)
            <?> "parenthesized expression"
        , parseIf <?> "if expression"
        , parseLetIn <?> "let binding"
        , parseMatch <?> "match expression"
        , parseRegionalExpr <?> "regional expression"
        , ConstExpr <$> parseConstant <?> "constant"
        , SpannedExpr
            <$> withSpan (parsePathBasedExpr allowStruct) <?> "variable or function call"
        , parseExprSeq <?> "expression sequence"
        ]

parseExpr :: Parser Expr
parseExpr = parseExprWithOpts True

parseExprWithOpts :: Bool -> Parser Expr
parseExprWithOpts allowStruct =
    (SpannedExpr <$> withSpan parseLambda)
        <|> makeExprParser
            (SpannedExpr <$> withSpan (parseExprTerm allowStruct))
            exprOpTable

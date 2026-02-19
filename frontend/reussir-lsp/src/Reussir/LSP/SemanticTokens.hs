{-# LANGUAGE OverloadedStrings #-}

{- |
  LSP Semantic Tokens
  ===================

  Provides semantic token computation for Reussir source files.
  Walks the parsed AST to collect tokens with their spans and types,
  then encodes them using the lsp library's helper functions.
-}
module Reussir.LSP.SemanticTokens (
    computeSemanticTokens,
) where

import Data.Int (Int64)
import Data.List (sortOn)
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Protocol.Types (SemanticTokenAbsolute (..), defaultSemanticTokensLegend, makeSemanticTokens)
import Reussir.Parser.Types.Expr qualified as Syn hiding (Named, Unnamed)
import Reussir.Parser.Types.Lexer (WithSpan (..))
import Reussir.Parser.Types.Lexer qualified as Syn
import Reussir.Parser.Types.Stmt qualified as Syn hiding (FlexFlag)
import Reussir.Parser.Types.Type qualified as Syn

-- | Compute semantic tokens for a parsed program.
-- Returns Nothing if encoding fails (should not happen with the default legend).
computeSemanticTokens :: T.Text -> [Syn.Stmt] -> Maybe LSP.SemanticTokens
computeSemanticTokens content stmts =
    let rawTokens = sortOn tokenSortKey $ concatMap (collectStmt content) stmts
     in case makeSemanticTokens defaultSemanticTokensLegend rawTokens of
            Left _ -> Nothing
            Right tokens -> Just tokens
  where
    tokenSortKey (SemanticTokenAbsolute l c _ _ _) = (l, c)

-- | Convert a byte offset to (line, column), both 0-indexed, as LSP UInt.
offsetToLineCol :: T.Text -> Int64 -> (LSP.UInt, LSP.UInt)
offsetToLineCol content offset =
    let prefix = T.take (fromIntegral offset) content
        line = T.count "\n" prefix
        col = case T.breakOnEnd "\n" prefix of
            ("", _) -> T.length prefix
            (before, _) -> T.length prefix - T.length before
     in (fromIntegral line, fromIntegral col)

-- | Make a semantic token from byte offsets.
mkToken :: T.Text -> Int64 -> Int64 -> LSP.SemanticTokenTypes -> [LSP.SemanticTokenModifiers] -> [SemanticTokenAbsolute]
mkToken content start end tokenType modifiers
    | end > start =
        let (line, col) = offsetToLineCol content start
            len = fromIntegral (end - start)
         in [SemanticTokenAbsolute line col len tokenType modifiers]
    | otherwise = []

-- | Collect tokens from a statement.
collectStmt :: T.Text -> Syn.Stmt -> [SemanticTokenAbsolute]
collectStmt content (Syn.SpannedStmt (WithSpan s _ _)) = collectStmt content s
collectStmt content (Syn.FunctionStmt f) = collectFunction content f
collectStmt content (Syn.RecordStmt r) = collectRecord content r
collectStmt content (Syn.ExternTrampolineStmt _ _ _ tyArgs) =
    concatMap (collectType content) tyArgs

-- | Collect tokens from a function definition.
collectFunction :: T.Text -> Syn.Function -> [SemanticTokenAbsolute]
collectFunction content f =
    concatMap (collectParam content) (Syn.funcParams f)
        ++ maybe [] (\(ty, _) -> collectType content ty) (Syn.funcReturnType f)
        ++ maybe [] (collectExpr content) (Syn.funcBody f)

-- | Collect tokens from a function parameter.
collectParam :: T.Text -> (Syn.Identifier, Syn.Type, Syn.FlexFlag) -> [SemanticTokenAbsolute]
collectParam content (_, ty, _) = collectType content ty

-- | Collect tokens from a record definition.
collectRecord :: T.Text -> Syn.Record -> [SemanticTokenAbsolute]
collectRecord content r = collectRecordFields content (Syn.recordFields r)

-- | Collect tokens from record fields.
collectRecordFields :: T.Text -> Syn.RecordFields -> [SemanticTokenAbsolute]
collectRecordFields content (Syn.Named fields) =
    concatMap (\(WithSpan (_, ty, _) _ _) -> collectType content ty) fields
collectRecordFields content (Syn.Unnamed fields) =
    concatMap (\(WithSpan (ty, _) _ _) -> collectType content ty) fields
collectRecordFields content (Syn.Variants variants) =
    concatMap (\(WithSpan (_, tyArgs) _ _) -> concatMap (collectType content) tyArgs) variants

-- | Collect tokens from a type annotation.
collectType :: T.Text -> Syn.Type -> [SemanticTokenAbsolute]
collectType content (Syn.TypeSpanned (WithSpan ty start end)) =
    case ty of
        Syn.TypeExpr _ args ->
            mkToken content start end LSP.SemanticTokenTypes_Type []
                ++ concatMap (collectType content) args
        Syn.TypeIntegral _ -> mkToken content start end LSP.SemanticTokenTypes_Type []
        Syn.TypeFP _ -> mkToken content start end LSP.SemanticTokenTypes_Type []
        Syn.TypeBool -> mkToken content start end LSP.SemanticTokenTypes_Type []
        Syn.TypeStr -> mkToken content start end LSP.SemanticTokenTypes_Type []
        Syn.TypeUnit -> mkToken content start end LSP.SemanticTokenTypes_Type []
        Syn.TypeBottom -> mkToken content start end LSP.SemanticTokenTypes_Type []
        Syn.TypeArrow a b -> collectType content a ++ collectType content b
        Syn.TypeSpanned _ -> collectType content ty
collectType content other = collectTypeInner content other

-- | Walk into type structure without the outer span.
collectTypeInner :: T.Text -> Syn.Type -> [SemanticTokenAbsolute]
collectTypeInner content (Syn.TypeExpr _ args) = concatMap (collectType content) args
collectTypeInner content (Syn.TypeArrow a b) = collectType content a ++ collectType content b
collectTypeInner content (Syn.TypeSpanned ws) = collectType content (Syn.TypeSpanned ws)
collectTypeInner _ _ = []

-- | Collect tokens from an expression.
collectExpr :: T.Text -> Syn.Expr -> [SemanticTokenAbsolute]
collectExpr content (Syn.SpannedExpr (WithSpan e _ _)) = collectExpr content e
collectExpr _ (Syn.ConstExpr _) = []
collectExpr content (Syn.BinOpExpr _ e1 e2) = collectExpr content e1 ++ collectExpr content e2
collectExpr content (Syn.UnaryOpExpr _ e) = collectExpr content e
collectExpr content (Syn.If cond thenE elseE) =
    collectExpr content cond ++ collectExpr content thenE ++ collectExpr content elseE
collectExpr content (Syn.Cast ty e) = collectType content ty ++ collectExpr content e
collectExpr content (Syn.Let _name mTy val) =
    maybe [] (\(ty, _) -> collectType content ty) mTy
        ++ collectExpr content val
collectExpr content (Syn.ExprSeq es) = concatMap (collectExpr content) es
collectExpr content (Syn.Lambda (Syn.LambdaExpr args body)) = 
    concatMap (\(_, ty) -> maybe [] (collectType content) ty) args ++ collectExpr content body
collectExpr content (Syn.Match scrutinee arms) =
    collectExpr content scrutinee
        ++ concatMap (\(pat, body) -> collectPattern content pat ++ collectExpr content body) arms
collectExpr _ (Syn.Var _) = []
collectExpr content (Syn.FuncCallExpr fc) = collectFuncCall content fc
collectExpr content (Syn.RegionalExpr e) = collectExpr content e
collectExpr content (Syn.CtorCallExpr cc) = collectCtorCall content cc
collectExpr content (Syn.AccessChain e _) = collectExpr content e
collectExpr content (Syn.Assign lhs _ rhs) = collectExpr content lhs ++ collectExpr content rhs

-- | Collect tokens from a function call.
collectFuncCall :: T.Text -> Syn.FuncCall -> [SemanticTokenAbsolute]
collectFuncCall content (Syn.FuncCall _ tyArgs args) =
    concatMap (maybe [] (collectType content)) tyArgs
        ++ concatMap (collectExpr content) args

-- | Collect tokens from a constructor call.
collectCtorCall :: T.Text -> Syn.CtorCall -> [SemanticTokenAbsolute]
collectCtorCall content (Syn.CtorCall _ tyArgs args) =
    concatMap (maybe [] (collectType content)) tyArgs
        ++ concatMap (\(_, e) -> collectExpr content e) args

-- | Collect tokens from a pattern.
collectPattern :: T.Text -> Syn.Pattern -> [SemanticTokenAbsolute]
collectPattern content (Syn.Pattern kind mGuard) =
    collectPatternKind content kind ++ maybe [] (collectExpr content) mGuard

-- | Collect tokens from a pattern kind.
collectPatternKind :: T.Text -> Syn.PatternKind -> [SemanticTokenAbsolute]
collectPatternKind _ Syn.WildcardPat = []
collectPatternKind _ (Syn.BindPat _) = []
collectPatternKind content (Syn.CtorPat _ args _ _) =
    concatMap (\arg -> collectPatternKind content (Syn.patCtorArgKind arg)) args
collectPatternKind _ (Syn.ConstPat _) = []

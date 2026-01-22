{-# LANGUAGE RecordWildCards #-}

module Reussir.Core.Full.Expr where

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Effectful (inject)
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Core.Data.Full.Context (FullContext (..), FullEff, LocalFullContext (..))
import Reussir.Core.Data.Full.Expr (Expr (..), ExprKind (..))
import Reussir.Core.Data.Full.Type (Type (..))
import Reussir.Core.Data.Semi.Expr qualified as SemiExpr
import Reussir.Core.Data.Semi.Type qualified as SemiType
import Reussir.Core.Data.UniqueID (ExprID (ExprID))
import Reussir.Core.Full.Context (addError, withSpan)
import Reussir.Core.Full.Type (convertSemiType)
import Reussir.Core.Semi.Mangle (mangleABIName)
import Reussir.Core.Semi.Type qualified as Semi (substituteGenericMap)
import Prelude hiding (span)

exprWithSpan :: Type -> ExprKind -> FullEff Expr
exprWithSpan exprType exprKind = do
    exprSpan <- State.gets currentSpan
    exprID <- ExprID <$> State.gets exprCounter
    State.modify $ \st -> st{exprCounter = exprCounter st + 1}
    return $ Expr{exprKind, exprSpan, exprType, exprID}

convertSemiExpr :: SemiExpr.Expr -> FullEff Expr
convertSemiExpr semiExpr = do
    let span = SemiExpr.exprSpan semiExpr
    withMaybeSpan span $ do
        ty <- convertTy (SemiExpr.exprType semiExpr)
        case SemiExpr.exprKind semiExpr of
            SemiExpr.GlobalStr token -> exprWithSpan ty $ GlobalStr token
            SemiExpr.Constant val -> exprWithSpan ty $ Constant val
            SemiExpr.Negate e ->
                Negate <$> convertSemiExpr e >>= exprWithSpan ty
            SemiExpr.Not e -> Not <$> convertSemiExpr e >>= exprWithSpan ty
            SemiExpr.Arith e1 op e2 ->
                Arith
                    <$> convertSemiExpr e1
                    <*> pure op
                    <*> convertSemiExpr e2
                    >>= exprWithSpan ty
            SemiExpr.Cmp e1 op e2 ->
                Cmp
                    <$> convertSemiExpr e1
                    <*> pure op
                    <*> convertSemiExpr e2
                    >>= exprWithSpan ty
            SemiExpr.Cast e t ->
                Cast
                    <$> convertSemiExpr e
                    <*> convertTy t
                    >>= exprWithSpan ty
            SemiExpr.ScfIfExpr cond t f ->
                ScfIfExpr
                    <$> convertSemiExpr cond
                    <*> convertSemiExpr t
                    <*> convertSemiExpr f
                    >>= exprWithSpan ty
            SemiExpr.Var varID -> exprWithSpan ty $ Var varID
            SemiExpr.RegionRun e ->
                RegionRun <$> convertSemiExpr e >>= exprWithSpan ty
            SemiExpr.Proj e indices ->
                Proj
                    <$> convertSemiExpr e
                    <*> pure indices
                    >>= exprWithSpan ty
            SemiExpr.Assign e1 idx e2 ->
                Assign
                    <$> convertSemiExpr e1
                    <*> pure idx
                    <*> convertSemiExpr e2
                    >>= exprWithSpan ty
            SemiExpr.Let{..} -> do
                varExpr' <- convertSemiExpr letVarExpr
                bodyExpr' <- convertSemiExpr letBodyExpr
                exprWithSpan ty $ Let letVarSpan letVarID letVarName varExpr' bodyExpr'
            SemiExpr.FuncCall{..} -> do
                gMap <- State.gets genericMap
                let instantiatedTyArgs = map (flip Semi.substituteGenericMap gMap) funcCallTyArgs
                let mangledName = mangleABIName (SemiType.TypeRecord funcCallTarget instantiatedTyArgs SemiType.Irrelevant)
                let symbol = verifiedSymbol mangledName
                args' <- mapM convertSemiExpr funcCallArgs
                exprWithSpan ty $ FuncCall symbol args' funcCallRegional
            SemiExpr.Poison -> exprWithSpan ty Poison
            SemiExpr.CompoundCall{..} -> do
                args' <- mapM convertSemiExpr compoundCallArgs
                hoistRcWrapping ty $ CompoundCall args'
            SemiExpr.VariantCall{..} -> do
                arg' <- convertSemiExpr variantCallArg
                hoistRcWrapping ty $ VariantCall variantCallVariant arg'
            SemiExpr.NullableCall e -> do
                e' <- mapM convertSemiExpr e
                exprWithSpan ty $ NullableCall e'
            SemiExpr.IntrinsicCall{..} -> do
                args' <- mapM convertSemiExpr intrinsicCallArgs
                exprWithSpan ty $ IntrinsicCall intrinsicCallTarget args'

hoistRcWrapping :: Type -> ExprKind -> FullEff Expr
hoistRcWrapping ty e = do
    case ty of
        TypeRc innerTy cap -> do
            e' <- exprWithSpan innerTy e
            exprWithSpan (TypeRc innerTy cap) (RcWrap e')
        _ -> exprWithSpan ty e

withMaybeSpan :: Maybe (Int64, Int64) -> FullEff a -> FullEff a
withMaybeSpan Nothing cont = cont
withMaybeSpan (Just span') cont = withSpan span' cont

convertTy :: SemiType.Type -> FullEff Type
convertTy t = do
    span <- State.gets currentSpan
    genericMap <- State.gets genericMap
    records <- State.gets @FullContext ctxSemiRecords
    res <- convertSemiType (fromMaybe (0, 0) span) genericMap records t
    case res of
        Left errs -> do
            mapM_ (inject . addError) errs
            pure TypeBottom -- Or TypePoison if available, TypeBottom is likely safest fallback
        Right ty -> pure ty

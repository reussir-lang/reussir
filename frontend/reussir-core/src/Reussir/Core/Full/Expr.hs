{-# LANGUAGE RecordWildCards #-}

module Reussir.Core.Full.Expr where

import Control.Monad
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Effectful (inject)
import Effectful.Prim.IORef.Strict (readIORef')
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Codegen.Type.Data (Capability (..))
import Reussir.Parser.Types.Lexer (WithSpan (..))
import Prelude hiding (span)

import Data.Vector.Strict qualified as V
import Effectful.State.Static.Local qualified as State

import Reussir.Core.Data.Full.Context (
    FullContext (..),
    FullEff,
    LocalFullContext (..),
 )
import Reussir.Core.Data.Full.Error (Error (..), ErrorKind (..))
import Reussir.Core.Data.Full.Expr (DTSwitchCases (..), DecisionTree (..), Expr (..), ExprKind (..), PatternVarRef (..))
import Reussir.Core.Data.Full.Type (Type (..))
import Reussir.Core.Data.UniqueID (ExprID (ExprID))
import Reussir.Core.Full.Context (addError, withSpan)
import Reussir.Core.Full.Type (convertSemiType)
import Reussir.Core.Semi.Mangle (mangleABIName)

import Data.IntMap.Strict qualified as IntMap
import Reussir.Core.Data.Semi.Expr qualified as SemiExpr
import Reussir.Core.Data.Semi.Record qualified as Semi
import Reussir.Core.Data.Semi.Type qualified as SemiType
import Reussir.Core.Semi.Type qualified as Semi (substituteGenericMap)
import Reussir.Core.Uitls.HashTable qualified as H

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
            -- Check e2 is indeed a Nullable<Rc<T, flex>>, where T is a record type with regional defaultCap,
            -- and the same as the raw field (not the wrapped field) at index idx of e1.
            SemiExpr.Assign e1 idx e2 -> do
                e1' <- convertSemiExpr e1
                e2' <- convertSemiExpr e2
                -- Validate e2's type is Nullable<Rc<T, Flex>>
                span' <- State.gets currentSpan
                let srcTy = exprType e2'
                case srcTy of
                    TypeNullable (TypeRc _ Flex) -> pure ()
                    TypeNullable (TypeRc _ _) -> do
                        inject $ addError (Error (fromMaybe (0, 0) span') InvalidAssignSourceCapability)
                    _ -> do
                        inject $
                            addError (Error (fromMaybe (0, 0) span') InvalidAssignSourceNotRegional)
                exprWithSpan ty $ Assign e1' idx e2'
            SemiExpr.Let{..} -> do
                varExpr' <- convertSemiExpr letVarExpr
                exprWithSpan ty $ Let letVarSpan letVarID letVarName varExpr'
            SemiExpr.FuncCall{..} -> do
                gMap <- State.gets genericMap
                let instantiatedTyArgs = map (flip Semi.substituteGenericMap gMap) funcCallTyArgs
                let mangledName =
                        mangleABIName
                            (SemiType.TypeRecord funcCallTarget instantiatedTyArgs SemiType.Irrelevant)
                let symbol = verifiedSymbol mangledName
                args' <- mapM convertSemiExpr funcCallArgs
                exprWithSpan ty $ FuncCall symbol args' funcCallRegional
            SemiExpr.Poison -> exprWithSpan ty Poison
            SemiExpr.CompoundCall{..} -> do
                semiRecords <- State.gets @FullContext ctxSemiRecords
                maybeRecord <- H.lookup semiRecords compoundCallTarget
                recordFields <-
                    maybe
                        (return Nothing)
                        (readIORef' . Semi.recordFields)
                        maybeRecord
                case recordFields of
                    Nothing -> error "record should be well-defined at this stage"
                    Just fields -> do
                        let fieldMutability = case fields of
                                Semi.Named v -> map (\(WithSpan (_, _, flag) _ _) -> flag) $ V.toList v
                                Semi.Unnamed v -> map (\(WithSpan (_, flag) _ _) -> flag) $ V.toList v
                                Semi.Variants _ -> repeat False
                        let convertFieldExpr expr mutability =
                                flexibleScope mutability $ convertSemiExpr expr
                        args' <- zipWithM convertFieldExpr compoundCallArgs fieldMutability
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
            SemiExpr.Sequence es -> do
                es' <- mapM convertSemiExpr es
                exprWithSpan ty $ Sequence es'
            SemiExpr.Match scrutinee dt -> do
                scrutinee' <- convertSemiExpr scrutinee
                dt' <- convertDecisionTree dt
                exprWithSpan ty $ Match scrutinee' dt'

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
    flexible <- State.gets @FullContext ctxFlexible
    case res of
        Left errs -> do
            mapM_ (inject . addError) errs
            pure TypeBottom -- Or TypePoison if available, TypeBottom is likely safest fallback
        Right (TypeRc innerTy Regional) | flexible -> pure (TypeRc innerTy Flex)
        Right (TypeNullable (TypeRc innerTy Regional)) | flexible -> pure (TypeNullable (TypeRc innerTy Flex))
        Right ty -> pure ty

flexibleScope :: Bool -> FullEff a -> FullEff a
flexibleScope flexible cont = do
    oldFlexible <- State.gets @FullContext ctxFlexible
    State.modify $ \st -> st{ctxFlexible = flexible}
    result <- cont
    State.modify $ \st -> st{ctxFlexible = oldFlexible}
    return result

convertDecisionTree :: SemiExpr.DecisionTree -> FullEff DecisionTree
convertDecisionTree SemiExpr.DTUncovered = pure DTUncovered
convertDecisionTree SemiExpr.DTUnreachable = pure DTUnreachable
convertDecisionTree (SemiExpr.DTLeaf body bindings) = do
    body' <- convertSemiExpr body
    pure $ DTLeaf body' (IntMap.map convertPatternVarRef bindings)
convertDecisionTree (SemiExpr.DTGuard bindings guardExpr trueBr falseBr) = do
    guard' <- convertSemiExpr guardExpr
    trueBr' <- convertDecisionTree trueBr
    falseBr' <- convertDecisionTree falseBr
    pure $ DTGuard (IntMap.map convertPatternVarRef bindings) guard' trueBr' falseBr'
convertDecisionTree (SemiExpr.DTSwitch varRef cases) = do
    cases' <- convertDTSwitchCases cases
    pure $ DTSwitch (convertPatternVarRef varRef) cases'

convertDTSwitchCases :: SemiExpr.DTSwitchCases -> FullEff DTSwitchCases
convertDTSwitchCases (SemiExpr.DTSwitchInt m def) = do
    m' <- mapM convertDecisionTree m
    def' <- convertDecisionTree def
    pure $ DTSwitchInt m' def'
convertDTSwitchCases (SemiExpr.DTSwitchBool t f) =
    DTSwitchBool <$> convertDecisionTree t <*> convertDecisionTree f
convertDTSwitchCases (SemiExpr.DTSwitchCtor cases) =
    DTSwitchCtor <$> V.mapM convertDecisionTree cases
convertDTSwitchCases (SemiExpr.DTSwitchString m def) = do
    m' <- mapM convertDecisionTree m
    def' <- convertDecisionTree def
    pure $ DTSwitchString m' def'
convertDTSwitchCases (SemiExpr.DTSwitchNullable j n) =
    DTSwitchNullable <$> convertDecisionTree j <*> convertDecisionTree n

convertPatternVarRef :: SemiExpr.PatternVarRef -> PatternVarRef
convertPatternVarRef (SemiExpr.PatternVarRef s) = PatternVarRef s

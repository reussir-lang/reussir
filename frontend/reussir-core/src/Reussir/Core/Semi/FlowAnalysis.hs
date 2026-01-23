{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Semi.FlowAnalysis where

import Control.Monad (forM_, zipWithM_)
import Data.HashTable.IO qualified as H
import Data.IntSet qualified as IntSet
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Effectful (liftIO)
import Effectful.Log qualified as L
import Effectful.Prim.IORef.Strict (readIORef')
import Effectful.State.Static.Local qualified as State
import Reussir.Core.Data.Generic (GenericSolution)
import Reussir.Core.Data.Semi.Context (GlobalSemiEff, SemiContext (..))
import Reussir.Core.Data.Semi.Expr (Expr (..), ExprKind (..))
import Reussir.Core.Data.Semi.Function (FunctionProto (..), FunctionTable (..))
import Reussir.Core.Data.Semi.Record (Record (..), RecordFields (..))
import Reussir.Core.Data.Semi.Type (Type (..))
import Reussir.Core.Data.UniqueID (GenericID (..))
import Reussir.Core.Generic (addConcreteFlow, addCtorLink, addDirectLink, solveGeneric)
import Reussir.Core.Semi.Context (addErrReport)
import Reussir.Core.Semi.Type (collectGenerics, isConcrete)
import Reussir.Diagnostic.Report (Report (..), defaultText)
import Reussir.Parser.Types.Lexer (Identifier, WithSpan (..))

-- Recursively analyze generic flow in an expression
-- We focus on function call and ctor call: at each call site, we examine:
-- 1. If a type fulfills isConcrete, we use addConcreteFlow to add this type as
--    a concrete instance to the generic variable.
-- 2. If otherwise, we add a flow edge from the types generics (collectGenerics) to the callee generic
--    i. if the type is directly a generic, we use addDirectLink
--    ii. otherwise we use addCtorLink
analyzeGenericFlowInExpr :: Expr -> GlobalSemiEff ()
analyzeGenericFlowInExpr expr = do
    case exprKind expr of
        FuncCall target tyArgs args _ -> do
            -- Analyze arguments
            mapM_ analyzeGenericFlowInExpr args

            -- Analyze generic flow
            functionTable <- State.gets functions
            liftIO (H.lookup (functionProtos functionTable) target) >>= \case
                Just proto -> do
                    let generics = funcGenerics proto
                    analyzeGenericInstantiationFlow generics tyArgs
                Nothing -> pure () -- Should have been caught by type checker
        CompoundCall target tyArgs args -> do
            -- Analyze arguments
            mapM_ analyzeGenericFlowInExpr args

            -- Analyze generic flow
            knownRecords <- State.gets knownRecords
            liftIO (H.lookup knownRecords target) >>= \case
                Just record -> do
                    let generics = recordTyParams record
                    analyzeGenericInstantiationFlow generics tyArgs
                Nothing -> pure () -- Should have been caught by type checker
        VariantCall target tyArgs _ arg -> do
            -- Analyze arguments
            analyzeGenericFlowInExpr arg

            -- Analyze generic flow
            knownRecords <- State.gets knownRecords
            liftIO (H.lookup knownRecords target) >>= \case
                Just record -> do
                    let generics = recordTyParams record
                    analyzeGenericInstantiationFlow generics tyArgs
                Nothing -> pure () -- Should have been caught by type checker

        -- Recursive cases
        Negate e -> analyzeGenericFlowInExpr e
        Not e -> analyzeGenericFlowInExpr e
        Arith e1 _ e2 -> analyzeGenericFlowInExpr e1 >> analyzeGenericFlowInExpr e2
        Cmp e1 _ e2 -> analyzeGenericFlowInExpr e1 >> analyzeGenericFlowInExpr e2
        Cast e _ -> analyzeGenericFlowInExpr e
        ScfIfExpr e1 e2 e3 -> do
            analyzeGenericFlowInExpr e1
            analyzeGenericFlowInExpr e2
            analyzeGenericFlowInExpr e3
        Proj e _ -> analyzeGenericFlowInExpr e
        Let _ _ _ val body -> do
            analyzeGenericFlowInExpr val
            analyzeGenericFlowInExpr body
        RegionRun e -> analyzeGenericFlowInExpr e
        -- Base cases
        GlobalStr _ -> pure ()
        Constant _ -> pure ()
        Var _ -> pure ()
        Poison -> pure ()
        NullableCall (Just e) -> analyzeGenericFlowInExpr e
        NullableCall Nothing -> pure ()
        Assign dst _ src -> analyzeGenericFlowInExpr dst >> analyzeGenericFlowInExpr src
        IntrinsicCall _ args -> mapM_ analyzeGenericFlowInExpr args

analyzeGenericInstantiationFlow ::
    [(Identifier, GenericID)] -> [Type] -> GlobalSemiEff ()
analyzeGenericInstantiationFlow genericParams tyArgs = do
    genericState <- State.gets generics
    zipWithM_
        ( \(_, gid) ty -> do
            if isConcrete ty
                then addConcreteFlow gid ty genericState
                else do
                    let srcGenerics = collectGenerics IntSet.empty ty
                    let srcGenericIDs = map (GenericID . fromIntegral) $ IntSet.toList srcGenerics
                    forM_ srcGenericIDs $ \srcID -> do
                        case ty of
                            TypeGeneric g | g == srcID -> addDirectLink srcID gid genericState
                            _ -> addCtorLink srcID gid ty genericState
        )
        genericParams
        tyArgs

analyzeGenericFlowInType :: Type -> GlobalSemiEff ()
analyzeGenericFlowInType (TypeRecord path args _) = do
    knownRecords <- State.gets knownRecords
    mRecord <- liftIO $ H.lookup knownRecords path
    case mRecord of
        Just record -> do
            let generics = recordTyParams record
            analyzeGenericInstantiationFlow generics args
        Nothing -> pure ()
    mapM_ analyzeGenericFlowInType args
analyzeGenericFlowInType (TypeClosure args ret) = do
    mapM_ analyzeGenericFlowInType args
    analyzeGenericFlowInType ret
analyzeGenericFlowInType (TypeNullable inner) =
    analyzeGenericFlowInType inner
analyzeGenericFlowInType _ = pure ()

analyzeGenericFlowInRecord :: Record -> GlobalSemiEff ()
analyzeGenericFlowInRecord record = do
    fieldsMaybe <- readIORef' (recordFields record)
    case fieldsMaybe of
        Just (Named fs) -> V.forM_ fs $ \(WithSpan (_, t, _) _ _) -> analyzeGenericFlowInType t
        Just (Unnamed fs) -> V.forM_ fs $ \(WithSpan (t, _) _ _) -> analyzeGenericFlowInType t
        -- no need to proceed to variants since variant share the same generics as parent record
        Just (Variants _) -> pure ()
        Nothing -> pure ()

-- Analyze generic flow for the whole translation module.
analyzeGenericFlow :: GlobalSemiEff ()
analyzeGenericFlow = do
    functionTable <- State.gets functions
    protos <- liftIO $ H.toList (functionProtos functionTable)
    forM_ protos $ \(_, proto) -> do
        forM_ (funcParams proto) $ \(_, paramType) -> do
            analyzeGenericFlowInType paramType
        analyzeGenericFlowInType (funcReturnType proto)
        mBody <- readIORef' (funcBody proto)
        case mBody of
            Just body -> analyzeGenericFlowInExpr body
            Nothing -> pure ()

    knownRecords <- State.gets knownRecords
    records <- liftIO $ H.toList knownRecords
    forM_ records $ \(_, record) -> analyzeGenericFlowInRecord record

solveAllGenerics :: GlobalSemiEff (Maybe GenericSolution)
solveAllGenerics = do
    L.logTrace_ "Solving all generics"
    analyzeGenericFlow
    genericState <- State.gets generics
    solveGeneric genericState >>= \case
        Right (x, y, ty) -> do
            -- TODO: better format report
            addErrReport $
                FormattedText $
                    [ defaultText $
                        "Growing edge detected between generic variables: "
                            <> T.pack (show x)
                            <> " -> "
                            <> T.pack (show y)
                            <> " via type "
                            <> T.pack (show ty)
                    ]
            return Nothing
        Left table -> do
            L.logInfo_ "Generic solving succeeded"
            return (Just table)

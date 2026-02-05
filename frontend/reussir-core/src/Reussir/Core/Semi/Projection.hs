{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Semi.Projection (
    resolveProjection,
    projectType,
) where

import Effectful (MonadIO (liftIO))
import Effectful.Prim.IORef.Strict (readIORef')
import Reussir.Parser.Types.Lexer (
    WithSpan (WithSpan),
    unIdentifier,
 )

import Data.HashTable.IO qualified as H
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Effectful.Log qualified as L
import Effectful.State.Static.Local qualified as State
import Reussir.Parser.Types.Capability qualified as Cap
import Reussir.Parser.Types.Expr qualified as Access (Access (..))

import Reussir.Core.Data.Semi.Context (
    SemiContext (..),
    SemiEff,
 )
import Reussir.Core.Data.Semi.Record (
    Record (..),
    RecordFields (..),
 )
import Reussir.Core.Data.Semi.Type (
    Type (..),
 )
import Reussir.Core.Data.UniqueID (GenericID (..))
import Reussir.Core.Semi.Context (
    addErrReportMsg,
 )
import Reussir.Core.Semi.Type (substituteGenericMap)

projectType :: Bool -> Type -> SemiEff Type
projectType nullable ty@(TypeRecord path _ _) = do
    record <- State.gets knownRecords
    record' <- liftIO $ H.lookup record path
    case record' of
        Nothing -> do
            addErrReportMsg $ "Unknown record: " <> T.pack (show path)
            return TypeBottom
        Just recordDef -> do
            let defaultCap = recordDefaultCap recordDef
            case (defaultCap, nullable) of
                (Cap.Regional, True) -> return $ TypeNullable ty
                (Cap.Regional, False) -> return ty
                (Cap.Shared, True) -> return $ TypeNullable ty
                (Cap.Shared, False) -> return ty
                (_, True) -> do
                    addErrReportMsg "Cannot make nullable value record"
                    return TypeBottom
                (_, False) -> return ty
projectType _ ty = return ty

-- | Resolve a single field access on a record type to its index and projected type
resolveProjection :: Type -> Access.Access -> SemiEff (Int, Type)
resolveProjection currentTy access = do
    case currentTy of
        TypeRecord path args _ -> do
            knownRecords <- State.gets knownRecords
            liftIO (H.lookup knownRecords path) >>= \case
                Just record -> do
                    let subst =
                            IntMap.fromList $
                                zip (map (\(_, GenericID gid) -> fromIntegral gid) $ recordTyParams record) args
                    fieldsMaybe <- readIORef' (recordFields record)
                    case (fieldsMaybe, access) of
                        (Just (Named fields), Access.Named name) -> do
                            case V.findIndex (\(WithSpan (n, _, _) _ _) -> n == name) fields of
                                Just idx -> do
                                    let WithSpan (_, fieldTy, nullable) _ _ = fields `V.unsafeIndex` idx
                                    fieldTy' <- projectType nullable $ substituteGenericMap fieldTy subst
                                    L.logTrace_ $ "Field type: " <> T.pack (show fieldTy')
                                    return (idx, fieldTy')
                                Nothing -> do
                                    addErrReportMsg $ "Field not found: " <> unIdentifier name
                                    return (-1, TypeBottom)
                        (Just (Unnamed fields), Access.Unnamed idx) -> do
                            let idxInt = fromIntegral idx
                            case fields V.!? idxInt of
                                Just (WithSpan (fieldTy, nullable) _ _) -> do
                                    fieldTy' <- projectType nullable $ substituteGenericMap fieldTy subst
                                    return (idxInt, fieldTy')
                                Nothing -> do
                                    addErrReportMsg $ "Field index out of bounds: " <> T.pack (show idx)
                                    return (-1, TypeBottom)
                        (Nothing, _) -> do
                            addErrReportMsg "Record fields not populated"
                            return (-1, TypeBottom)
                        _ -> do
                            addErrReportMsg "Invalid access type for record kind"
                            return (-1, TypeBottom)
                Nothing -> do
                    addErrReportMsg $ "Unknown record: " <> T.pack (show path)
                    return (-1, TypeBottom)
        _ -> do
            addErrReportMsg "Accessing field of non-record type"
            return (-1, TypeBottom)

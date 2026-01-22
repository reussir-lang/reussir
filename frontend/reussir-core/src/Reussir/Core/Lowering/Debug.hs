{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Lowering.Debug where

import Data.HashTable.IO qualified as H
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Reader.Static qualified as Reader
import Reussir.Codegen.Location qualified as DBG
import Reussir.Codegen.Type.Data qualified as IRType
import Reussir.Core.Data.FP (FloatingPointType (..))
import Reussir.Core.Data.Full.Record (
    Record (..),
    RecordFields (..),
    RecordKind (..),
 )
import Reussir.Core.Data.Full.Type (Type (..))
import Reussir.Core.Data.Integral (IntegralType (..))
import Reussir.Core.Data.Lowering.Context (LoweringContext (..))
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..))

-- Helper for unmangled name
unmangledPath :: Path -> T.Text
unmangledPath (Path name components) =
    T.intercalate "::" (map unIdentifier components ++ [unIdentifier name])

typeAsDbgType ::
    (IOE :> es, Reader.Reader LoweringContext :> es) => Type -> Eff es (Maybe DBG.DBGType)
typeAsDbgType ty = case ty of
    TypeIntegral (Signed w) -> pure $ do
        prim <- convertIntegralToPrim $ fromIntegral w
        pure $ DBG.Signed prim (T.pack $ "i" ++ show w)
    TypeIntegral (Unsigned w) -> pure $ do
        prim <- convertIntegralToPrim $ fromIntegral w
        pure $ DBG.Unsigned prim (T.pack $ "u" ++ show w)
    TypeFP fpt -> pure $ do
        prim <- convertFloatToPrim fpt
        pure $ DBG.FP prim (T.pack $ show fpt)
    TypeRecord sym -> do
        recordTable <- Reader.asks recordInstances
        record <- liftIO $ H.lookup recordTable sym
        case record of
            Nothing -> pure Nothing
            Just rec -> do
                let rawPath = recordRawPath rec
                fields <- case recordFields rec of
                    Components fs -> do
                        let mapped =
                                V.toList $
                                    V.imap
                                        (\i (id', t, _) -> (fromMaybe (T.pack $ show i) (fmap unIdentifier id'), t))
                                        fs
                        mapM processField mapped
                    Variants vs -> do
                        -- vs is Vector Symbol
                        mapped <- V.forM vs $ \vSym -> do
                            mVarRec <- liftIO $ H.lookup recordTable vSym
                            case mVarRec of
                                Nothing -> pure Nothing
                                Just varRec -> do
                                    let Path name _ = recordRawPath varRec
                                    pure $ Just (unIdentifier name, TypeRecord vSym)

                        let validMapped = catMaybes $ V.toList mapped
                        mapM processField validMapped

                if any isNothing fields
                    then pure Nothing
                    else
                        pure $
                            Just $
                                DBG.Record
                                    { DBG.dbgRecordName = unmangledPath rawPath
                                    , DBG.dbgRecordFields = catMaybes fields
                                    , DBG.dbgRecordRep = sym -- sym is the Symbol
                                    , DBG.dbgRecordIsVariant = case recordKind rec of
                                        EnumKind -> True
                                        _ -> False
                                    }
    _ -> pure Nothing
  where
    processField (name, fieldTy) = do
        mDbgTy <- typeAsDbgType fieldTy
        pure $ fmap (\d -> (name, d)) mDbgTy

    isNothing Nothing = True
    isNothing _ = False

    convertIntegralToPrim :: Int -> Maybe IRType.PrimitiveInt
    convertIntegralToPrim 8 = Just IRType.PrimInt8
    convertIntegralToPrim 16 = Just IRType.PrimInt16
    convertIntegralToPrim 32 = Just IRType.PrimInt32
    convertIntegralToPrim 64 = Just IRType.PrimInt64
    convertIntegralToPrim 128 = Just IRType.PrimInt128
    convertIntegralToPrim _ = Nothing

    convertFloatToPrim :: FloatingPointType -> Maybe IRType.PrimitiveFloat
    convertFloatToPrim (IEEEFloat 16) = Just IRType.PrimFloat16
    convertFloatToPrim (IEEEFloat 32) = Just IRType.PrimFloat32
    convertFloatToPrim (IEEEFloat 64) = Just IRType.PrimFloat64
    convertFloatToPrim BFloat16 = Just IRType.PrimBFloat16
    convertFloatToPrim Float8 = Just IRType.PrimFloat8
    convertFloatToPrim _ = Nothing

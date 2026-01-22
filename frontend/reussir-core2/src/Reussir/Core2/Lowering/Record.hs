{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core2.Lowering.Record where

import Control.Monad (forM)
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Type qualified as IR
import Reussir.Codegen.Type.Record qualified as IRRecord
import Reussir.Core2.Data.Full.Record qualified as Full
import Reussir.Core2.Data.Lowering.Context (GlobalLoweringEff)
import Reussir.Core2.Lowering.Type (convertType)

lowerRecord :: Full.Record -> GlobalLoweringEff ()
lowerRecord record = do
    let symbol = Full.recordName record
    let semKind = Full.recordKind record

    let irKind = case semKind of
            Full.StructKind -> IRRecord.Compound
            Full.EnumKind -> IRRecord.Variant
            Full.EnumVariant{} -> IRRecord.Compound

    irFields <- case (semKind, Full.recordFields record) of
        (Full.StructKind, Full.Components fields) ->
            forM fields $ \(_, ty, mutable) -> do
                irTy <- convertType ty
                pure $ IRRecord.RecordField irTy mutable
        (Full.EnumKind, Full.Variants variants) ->
            forM variants $ \sym ->
                pure $ IRRecord.RecordField (IR.TypeExpr sym) False
        (Full.EnumVariant{}, Full.Components fields) ->
            forM fields $ \(_, ty, mutable) -> do
                irTy <- convertType ty
                pure $ IRRecord.RecordField irTy mutable
        _ -> error $ "Mismatched record kind and fields: " ++ show semKind ++ " " ++ show (Full.recordFields record)

    let irRecord =
            IRRecord.Record
                { IRRecord.defaultCapability = Full.recordDefaultCap record
                , IRRecord.fields = irFields
                , IRRecord.kind = irKind
                }

    mod' <- State.get
    let updatedMod = mod'{IR.recordInstances = IR.RecordInstance (symbol, irRecord) : IR.recordInstances mod'}
    State.put updatedMod

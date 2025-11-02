{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Codegen.Type.Emission (emitRecord) where

import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Codegen.Context.Codegen
import Reussir.Codegen.Context.Emission (Emission (emit))
import Reussir.Codegen.Type.Data (
    Capability (..),
    Primitive (..),
    PrimitiveFloat (..),
    PrimitiveInt (..),
    Type (..),
 )
import Reussir.Codegen.Type.Mangle (mangleTypeWithPrefix)
import Reussir.Codegen.Type.Record (Record (..), RecordField, RecordKind (..))

instance Emission PrimitiveInt where
    emit PrimInt8 = pure "i8"
    emit PrimInt16 = pure "i16"
    emit PrimInt32 = pure "i32"
    emit PrimInt64 = pure "i64"
    emit PrimInt128 = pure "i128"
    emit PrimIndex = pure "index"

instance Emission PrimitiveFloat where
    emit PrimFloat8 = pure "f8"
    emit PrimFloat16 = pure "f16"
    emit PrimBFloat16 = pure "bf16"
    emit PrimFloat32 = pure "f32"
    emit PrimFloat64 = pure "f64"
    emit PrimFloat128 = pure "f128"

instance Emission Primitive where
    emit (PrimInt i) = emit i
    emit (PrimFloat f) = emit f
    emit PrimBool = pure "i1"
    emit PrimUnit = pure "()"

emitTy :: Bool -> Type -> Codegen TB.Builder
emitTy _ (TypePrim prim) = emit prim
emitTy _ (TypeRc _rc) = error "Emission for Rc not yet implemented"
emitTy _ (TypeRef _ref) = error "Emission for Ref not yet implemented"
emitTy _ (TypeClosure _closure) = error "Emission for Closure not yet implemented"
emitTy _ (TypeTensor _tensor) = error "Emission for Tensor not yet implemented"
emitTy toplevel ty@(TypeExpr expr) = do
    record <- getRecord expr
    case record of
        Just r -> emitRecord toplevel (Just $ TB.toLazyText $ mangleTypeWithPrefix ty) r
        Nothing -> error "Record not found for expression"

instance Emission Type where
    emit = emitTy True

instance Emission RecordKind where
    emit Compound = pure "compound"
    emit Variant = pure "variant"

-- !reussir.record<compound "List::Cons" { i32, [shared] !list_incomplete }>
-- !reussir.record<compound "List::Nil" {}>

emitRecord :: Bool -> Maybe T.Text -> Record -> Codegen TB.Builder
emitRecord
    toplevel -- if this is toplevel, current emission must proceed
    name
    Record
        { kind
        , fields
        , defaultCapability
        } = do
        -- Check if we should use guard (for recursive calls or already complete)
        guard' <- if toplevel then pure Nothing else emitRecordGuard name
        case guard' of
            Just finished -> pure finished
            Nothing -> do
                -- Set state to Incomplete before emitting (for recursive protection)
                case name of
                    Nothing -> pure ()
                    Just n -> setRecordEmissionState n RecordEmissionIncomplete
                doEmit
      where
        -- check if we can directly use alias or we really need to emit a record
        emitRecordGuard :: Maybe T.Text -> Codegen (Maybe TB.Builder)
        emitRecordGuard Nothing = pure Nothing
        emitRecordGuard (Just n) = do
            status <- getRecordEmissionState n
            case status of
                RecordEmissionComplete -> pure $ Just $ "!" <> TB.fromLazyText n
                RecordEmissionIncomplete -> do
                    kind' <- emit kind
                    pure $ Just $ "!reussir.record<" <> kind' <> " " <> TB.fromString (show n) <> ">"
                RecordEmissionPending -> pure Nothing

        translateCapability :: Capability -> TB.Builder
        translateCapability Shared = "[shared]"
        translateCapability Value = "[value]"
        translateCapability Unspecified = ""
        translateCapability Flex = "[flex]"
        translateCapability Rigid = "[rigid]"

        emitField :: RecordField -> Codegen TB.Builder
        emitField (field, capability) = do
            let capability' = translateCapability capability
            field' <- emitTy False field
            if capability == Unspecified
                then pure field'
                else pure $ capability' <> " " <> field'

        doEmit :: Codegen TB.Builder
        doEmit = do
            name' <- case name of
                Just n -> pure $ " \"" <> TB.fromLazyText n <> "\" "
                Nothing -> pure " "
            kind' <- emit kind
            let defaultCapability' = translateCapability defaultCapability
            fields' <- mapM (fmap TB.toLazyText . emitField) fields
            let concatFields' = (TB.fromLazyText $ T.intercalate ", " fields')
            pure $ "!reussir.record<" <> kind' <> name' <> defaultCapability' <> "{" <> concatFields' <> "}>"

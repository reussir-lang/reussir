{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Codegen.Type.Emission (emitRecord) where

import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Reussir.Codegen.Context.Codegen
import Reussir.Codegen.Context.Emission (Emission (emit), intercalate)
import Reussir.Codegen.Type.Data (
    Atomicity (..),
    Capability (..),
    Closure (..),
    Primitive (..),
    PrimitiveFloat (..),
    PrimitiveInt (..),
    Rc (..),
    Ref (..),
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

-- Helper functions for emitting capabilities and atomicity
emitCapability :: Capability -> TB.Builder
emitCapability Shared = " shared"
emitCapability Value = " value"
emitCapability Unspecified = ""
emitCapability Flex = " flex"
emitCapability Rigid = " rigid"
emitCapability Field = " field"

emitAtomicity :: Atomicity -> TB.Builder
emitAtomicity NonAtomic = " normal"
emitAtomicity Atomic = " atomic"

-- Notice that toplevel is with respect to record emission. So for other types, the function should pass through the toplevel flag.
emitTy :: Bool -> Type -> Codegen TB.Builder
emitTy _ (TypePrim prim) = emit prim
emitTy toplevel (TypeRc (Rc ty atm cap)) = do
    ty' <- emitTy toplevel ty
    pure $ "!reussir.rc<" <> ty' <> emitCapability cap <> emitAtomicity atm <> ">"
-- !reussir.ref<index capability atomic>
emitTy toplevel (TypeRef (Ref ty atm cap)) = do
    ty' <- emitTy toplevel ty
    pure $ "!reussir.ref<" <> ty' <> emitCapability cap <> emitAtomicity atm <> ">"
-- !reussir.closure<(i32) -> i32>
emitTy toplevel (TypeClosure (Closure args returnTy)) = do
    args' <- mapM (emitTy toplevel) args
    let concatArgs' = intercalate ", " args'
    case returnTy of
        TypePrim PrimUnit -> pure $ "!reussir.closure<(" <> concatArgs' <> ")>"
        _ -> do
            returnTy' <- emitTy toplevel returnTy
            pure $ "!reussir.closure<(" <> concatArgs' <> ") -> " <> returnTy' <> ">"
emitTy toplevel ty@(TypeExpr expr) = do
    record <- getRecord expr
    case record of
        Just r -> emitRecord toplevel (Just $ TB.runBuilder $ mangleTypeWithPrefix ty) r
        Nothing -> error "Record not found for expression"
emitTy toplevel (TypeNullable ty) = do
    ty' <- emitTy toplevel ty
    pure $ "!reussir.nullable<" <> ty' <> ">"
emitTy _ TypeRegion = pure "!reussir.region"
-- TODO: backend does not support tensor emission yet
emitTy _ (TypeTensor _tensor) = error "Emission for Tensor not yet implemented"

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
                RecordEmissionComplete -> pure $ Just $ "!" <> TB.fromText n
                RecordEmissionIncomplete -> do
                    kind' <- emit kind
                    pure $ Just $ "!reussir.record<" <> kind' <> " " <> fromString (show n) <> ">"
                RecordEmissionPending -> pure Nothing

        translateCapability :: Capability -> TB.Builder
        translateCapability Shared = "[shared]"
        translateCapability Value = "[value]"
        translateCapability Unspecified = ""
        translateCapability Flex = "[flex]"
        translateCapability Rigid = "[rigid]"
        translateCapability Field = "[field]"

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
                Just n -> pure $ " \"" <> TB.fromText n <> "\" "
                Nothing -> pure " "
            kind' <- emit kind
            let defaultCapability' = translateCapability defaultCapability
            fields' <- mapM emitField fields
            let concatFields' = intercalate ", " fields'
            pure $ "!reussir.record<" <> kind' <> name' <> defaultCapability' <> "{" <> concatFields' <> "}>"

{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Lowering.Type where

import Effectful (liftIO)
import Reussir.Codegen.Context.Symbol (verifiedSymbol, symbolText)
import Reussir.Codegen.Type qualified as IR

import Reussir.Core.Data.Lowering.Context (GlobalLoweringEff, LoweringContext(..))

import Data.HashTable.IO qualified as H
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Effectful.Reader.Static qualified as Reader

import Reussir.Core.Data.FP qualified as FP
import Reussir.Core.Data.Full.Record qualified as Full
import Reussir.Core.Data.Full.Type qualified as Full
import Reussir.Core.Data.Integral qualified as Int
import Reussir.Core.Data.Semi.Type qualified as Semi
import Reussir.Core.Semi.Mangle (mangleABIName)

convertType :: Full.Type -> GlobalLoweringEff IR.Type
convertType Full.TypeBool = pure $ IR.TypePrim IR.PrimBool
convertType Full.TypeUnit = pure $ IR.TypePrim IR.PrimUnit
convertType (Full.TypeIntegral (Int.Signed w)) = pure $ convertIntegral (fromIntegral w)
convertType (Full.TypeIntegral (Int.Unsigned w)) = pure $ convertIntegral (fromIntegral w)
convertType (Full.TypeFP (FP.IEEEFloat w)) = pure $ convertFloat (fromIntegral w)
convertType (Full.TypeFP FP.BFloat16) = pure $ IR.TypePrim (IR.PrimFloat IR.PrimBFloat16)
convertType (Full.TypeFP FP.Float8) = pure $ IR.TypePrim (IR.PrimFloat IR.PrimFloat8)
convertType (Full.TypeClosure args ret) = do
    (IR.TypeClosure .) . IR.Closure <$> mapM convertType args <*> convertType ret
convertType (Full.TypeNullable inner) = IR.TypeNullable <$> convertType inner
convertType (Full.TypeRecord symbol) = do
    ctx <- Reader.ask
    mRecord <- liftIO $ H.lookup (recordInstances ctx) symbol
    case mRecord of
        Just record | Full.ExternStructKind <- Full.recordKind record -> do
            let rawPath = Full.recordRawPath record
            case HashMap.lookup rawPath (externStructs ctx) of
                Just template -> do
                    let tyArgs = Full.recordSemiTyParams record
                    let ffiName = substituteTemplate template tyArgs
                    let dtorName = symbolText symbol <> "$polyffi_drop"
                    let dtorSym = verifiedSymbol dtorName
                    pure $ IR.TypeFFIObject ffiName dtorSym
                Nothing -> pure $ IR.TypeExpr symbol
        _ -> pure $ IR.TypeExpr symbol
convertType (Full.TypeRc ty cap) = do
    inner <- convertType ty
    pure $
        IR.TypeRc
            IR.Rc
                { IR.rcBoxInner = inner
                , IR.rcBoxCapability = cap
                , IR.rcBoxAtomicity = IR.NonAtomic -- TODO: implement atomicity
                }
convertType Full.TypeStr = pure (IR.TypeStr IR.Global)
convertType Full.TypeBottom = error "NYI: TypeBottom"

convertIntegral :: Int -> IR.Type
convertIntegral 8 = IR.TypePrim (IR.PrimInt IR.PrimInt8)
convertIntegral 16 = IR.TypePrim (IR.PrimInt IR.PrimInt16)
convertIntegral 32 = IR.TypePrim (IR.PrimInt IR.PrimInt32)
convertIntegral 64 = IR.TypePrim (IR.PrimInt IR.PrimInt64)
convertIntegral 128 = IR.TypePrim (IR.PrimInt IR.PrimInt128)
convertIntegral w = error $ "Unsupported integer width: " ++ show w

convertFloat :: Int -> IR.Type
convertFloat 16 = IR.TypePrim (IR.PrimFloat IR.PrimFloat16)
convertFloat 32 = IR.TypePrim (IR.PrimFloat IR.PrimFloat32)
convertFloat 64 = IR.TypePrim (IR.PrimFloat IR.PrimFloat64)
convertFloat 128 = IR.TypePrim (IR.PrimFloat IR.PrimFloat128)
convertFloat w = error $ "Unsupported float width: " ++ show w

mkRefType :: IR.Type -> IR.Capability -> IR.Type
mkRefType ty cap = IR.TypeRef $ IR.Ref ty IR.NonAtomic cap

-- | Substitute ${T}, ${A}, ${B}, ... in a foreign type template with concrete
-- Rust type names from the instantiated type arguments.
substituteTemplate :: T.Text -> [Semi.Type] -> T.Text
substituteTemplate tpl tyArgs =
    foldl (\t (name, ty) -> T.replace ("${" <> name <> "}") (semiTypeToRustName ty) t)
        tpl (zip positionalNames tyArgs)
  where
    positionalNames :: [T.Text]
    positionalNames = ["T", "A", "B", "C", "D", "E", "F", "G"]

semiTypeToRustName :: Semi.Type -> T.Text
semiTypeToRustName (Semi.TypeIntegral (Int.Signed 8)) = "i8"
semiTypeToRustName (Semi.TypeIntegral (Int.Signed 16)) = "i16"
semiTypeToRustName (Semi.TypeIntegral (Int.Signed 32)) = "i32"
semiTypeToRustName (Semi.TypeIntegral (Int.Signed 64)) = "i64"
semiTypeToRustName (Semi.TypeIntegral (Int.Unsigned 8)) = "u8"
semiTypeToRustName (Semi.TypeIntegral (Int.Unsigned 16)) = "u16"
semiTypeToRustName (Semi.TypeIntegral (Int.Unsigned 32)) = "u32"
semiTypeToRustName (Semi.TypeIntegral (Int.Unsigned 64)) = "u64"
semiTypeToRustName (Semi.TypeFP (FP.IEEEFloat 32)) = "f32"
semiTypeToRustName (Semi.TypeFP (FP.IEEEFloat 64)) = "f64"
semiTypeToRustName Semi.TypeBool = "bool"
semiTypeToRustName Semi.TypeUnit = "()"
semiTypeToRustName ty = mangleABIName ty

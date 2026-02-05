module Reussir.Core.Lowering.Type where

import Reussir.Codegen.Type qualified as IR

import Reussir.Core.Data.Lowering.Context (GlobalLoweringEff)

import Reussir.Core.Data.FP qualified as FP
import Reussir.Core.Data.Full.Type qualified as Full
import Reussir.Core.Data.Integral qualified as Int

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
    pure $ IR.TypeExpr symbol
convertType (Full.TypeRc ty cap) = do
    inner <- convertType ty
    pure $
        IR.TypeRc
            IR.Rc
                { IR.rcBoxInner = inner
                , IR.rcBoxCapability = cap
                , IR.rcBoxAtomicity = IR.NonAtomic -- TODO: implement atomicity
                }
convertType Full.TypeStr = pure IR.TypeStr
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

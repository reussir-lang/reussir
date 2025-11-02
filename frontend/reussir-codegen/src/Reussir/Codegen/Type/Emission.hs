{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reussir.Codegen.Type.Emission where

import Reussir.Codegen.Context (Emission (emit))
import Reussir.Codegen.Type.Data

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
  emit (PrimInt bits) = emit bits
  emit (PrimFloat pft) = emit pft
  emit PrimBool = pure "i1"
  emit PrimUnit = pure "none"

instance Emission Type where
  emit (TypePrim prim) = emit prim
  emit _ = error "Emission for non-primitive types not implemented yet"
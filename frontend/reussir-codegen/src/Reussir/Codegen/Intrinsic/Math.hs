{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Intrinsic.Math
  ( Math (..),
    mathCodegen,
  )
where

import Control.Monad (unless)
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Codegen.Context qualified as C
import Reussir.Codegen.Intrinsic.Arith (FastMathFlag (..))
import Reussir.Codegen.Value (TypedValue)

fmfIsNone :: FastMathFlag -> Bool
fmfIsNone (FastMathFlag 0) = True
fmfIsNone _ = False

data Math
  = Absf FastMathFlag
  | Absi
  | Acos FastMathFlag
  | Acosh FastMathFlag
  | Asin FastMathFlag
  | Asinh FastMathFlag
  | Atan FastMathFlag
  | Atan2 FastMathFlag
  | Atanh FastMathFlag
  | Cbrt FastMathFlag
  | Ceil FastMathFlag
  | Copysign FastMathFlag
  | Cos FastMathFlag
  | Cosh FastMathFlag
  | Ctlz
  | Ctpop
  | Cttz
  | Erf FastMathFlag
  | Erfc FastMathFlag
  | Exp FastMathFlag
  | Exp2 FastMathFlag
  | Expm1 FastMathFlag
  | Floor FastMathFlag
  | Fma FastMathFlag
  | Fpowi FastMathFlag
  | Ipowi
  | Isfinite FastMathFlag
  | Isinf FastMathFlag
  | Isnan FastMathFlag
  | Isnormal FastMathFlag
  | Log10 FastMathFlag
  | Log1p FastMathFlag
  | Log2 FastMathFlag
  | Powf FastMathFlag
  | Round FastMathFlag
  | Roundeven FastMathFlag
  | Rsqrt FastMathFlag
  | Sin FastMathFlag
  | Sincos FastMathFlag
  | Sinh FastMathFlag
  | Sqrt FastMathFlag
  | Tan FastMathFlag
  | Tanh FastMathFlag
  | Trunc FastMathFlag
  deriving (Eq, Show)

fmfCodegen :: FastMathFlag -> C.Codegen ()
fmfCodegen fmf = unless (fmfIsNone fmf) $ do
  C.emitBuilder $ " fastmath<" <> C.emit fmf <> ">"

unaryFloatMathCodegen :: TB.Builder -> FastMathFlag -> TypedValue -> TypedValue -> C.Codegen ()
unaryFloatMathCodegen mnemonic fmf (inVal, _inTy) (resVal, resTy) = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "math." <> mnemonic <> " "
  C.emitBuilder $ C.emit inVal
  fmfCodegen fmf
  C.emitBuilder $ " : " <> C.emit resTy

unaryIntMathCodegen :: TB.Builder -> TypedValue -> TypedValue -> C.Codegen ()
unaryIntMathCodegen mnemonic (inVal, _inTy) (resVal, resTy) = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "math." <> mnemonic <> " "
  C.emitBuilder $ C.emit inVal
  C.emitBuilder $ " : " <> C.emit resTy

binaryFloatMathCodegen :: TB.Builder -> FastMathFlag -> TypedValue -> TypedValue -> TypedValue -> C.Codegen ()
binaryFloatMathCodegen mnemonic fmf (vA, _) (vB, _) (resVal, resTy) = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "math." <> mnemonic <> " "
  C.emitBuilder $ C.emit vA <> ", " <> C.emit vB
  fmfCodegen fmf
  C.emitBuilder $ " : " <> C.emit resTy

ternaryFloatMathCodegen :: TB.Builder -> FastMathFlag -> TypedValue -> TypedValue -> TypedValue -> TypedValue -> C.Codegen ()
ternaryFloatMathCodegen mnemonic fmf (vA, _) (vB, _) (vC, _) (resVal, resTy) = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "math." <> mnemonic <> " "
  C.emitBuilder $ C.emit vA <> ", " <> C.emit vB <> ", " <> C.emit vC
  fmfCodegen fmf
  C.emitBuilder $ " : " <> C.emit resTy

binaryIntMathCodegen :: TB.Builder -> TypedValue -> TypedValue -> TypedValue -> C.Codegen ()
binaryIntMathCodegen mnemonic (vA, _) (vB, _) (resVal, resTy) = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "math." <> mnemonic <> " "
  C.emitBuilder $ C.emit vA <> ", " <> C.emit vB
  C.emitBuilder $ " : " <> C.emit resTy

-- | Generate MLIR assembly for math operations.
-- This function dispatches to the appropriate code generator based on the operation type.
mathCodegen :: Math -> [TypedValue] -> [TypedValue] -> C.Codegen ()
-- ============================================================================
-- Unary Floating-Point Operations (with FastMathFlag)
-- ============================================================================

mathCodegen (Absf fmf) [inVal] [res] = unaryFloatMathCodegen "absf" fmf inVal res
mathCodegen (Acos fmf) [inVal] [res] = unaryFloatMathCodegen "acos" fmf inVal res
mathCodegen (Acosh fmf) [inVal] [res] = unaryFloatMathCodegen "acosh" fmf inVal res
mathCodegen (Asin fmf) [inVal] [res] = unaryFloatMathCodegen "asin" fmf inVal res
mathCodegen (Asinh fmf) [inVal] [res] = unaryFloatMathCodegen "asinh" fmf inVal res
mathCodegen (Atan fmf) [inVal] [res] = unaryFloatMathCodegen "atan" fmf inVal res
mathCodegen (Atanh fmf) [inVal] [res] = unaryFloatMathCodegen "atanh" fmf inVal res
mathCodegen (Cbrt fmf) [inVal] [res] = unaryFloatMathCodegen "cbrt" fmf inVal res
mathCodegen (Ceil fmf) [inVal] [res] = unaryFloatMathCodegen "ceil" fmf inVal res
mathCodegen (Cos fmf) [inVal] [res] = unaryFloatMathCodegen "cos" fmf inVal res
mathCodegen (Cosh fmf) [inVal] [res] = unaryFloatMathCodegen "cosh" fmf inVal res
mathCodegen (Erf fmf) [inVal] [res] = unaryFloatMathCodegen "erf" fmf inVal res
mathCodegen (Erfc fmf) [inVal] [res] = unaryFloatMathCodegen "erfc" fmf inVal res
mathCodegen (Exp fmf) [inVal] [res] = unaryFloatMathCodegen "exp" fmf inVal res
mathCodegen (Exp2 fmf) [inVal] [res] = unaryFloatMathCodegen "exp2" fmf inVal res
mathCodegen (Expm1 fmf) [inVal] [res] = unaryFloatMathCodegen "expm1" fmf inVal res
mathCodegen (Floor fmf) [inVal] [res] = unaryFloatMathCodegen "floor" fmf inVal res
mathCodegen (Isfinite fmf) [inVal] [res] = unaryFloatMathCodegen "isfinite" fmf inVal res
mathCodegen (Isinf fmf) [inVal] [res] = unaryFloatMathCodegen "isinf" fmf inVal res
mathCodegen (Isnan fmf) [inVal] [res] = unaryFloatMathCodegen "isnan" fmf inVal res
mathCodegen (Isnormal fmf) [inVal] [res] = unaryFloatMathCodegen "isnormal" fmf inVal res
mathCodegen (Log10 fmf) [inVal] [res] = unaryFloatMathCodegen "log10" fmf inVal res
mathCodegen (Log1p fmf) [inVal] [res] = unaryFloatMathCodegen "log1p" fmf inVal res
mathCodegen (Log2 fmf) [inVal] [res] = unaryFloatMathCodegen "log2" fmf inVal res
mathCodegen (Round fmf) [inVal] [res] = unaryFloatMathCodegen "round" fmf inVal res
mathCodegen (Roundeven fmf) [inVal] [res] = unaryFloatMathCodegen "roundeven" fmf inVal res
mathCodegen (Rsqrt fmf) [inVal] [res] = unaryFloatMathCodegen "rsqrt" fmf inVal res
mathCodegen (Sin fmf) [inVal] [res] = unaryFloatMathCodegen "sin" fmf inVal res
mathCodegen (Sinh fmf) [inVal] [res] = unaryFloatMathCodegen "sinh" fmf inVal res
mathCodegen (Sqrt fmf) [inVal] [res] = unaryFloatMathCodegen "sqrt" fmf inVal res
mathCodegen (Tan fmf) [inVal] [res] = unaryFloatMathCodegen "tan" fmf inVal res
mathCodegen (Tanh fmf) [inVal] [res] = unaryFloatMathCodegen "tanh" fmf inVal res
mathCodegen (Trunc fmf) [inVal] [res] = unaryFloatMathCodegen "trunc" fmf inVal res

-- ============================================================================
-- Unary Integer Operations (without FastMathFlag)
-- ============================================================================

mathCodegen Absi [inVal] [res] = unaryIntMathCodegen "absi" inVal res
mathCodegen Ctlz [inVal] [res] = unaryIntMathCodegen "ctlz" inVal res
mathCodegen Ctpop [inVal] [res] = unaryIntMathCodegen "ctpop" inVal res
mathCodegen Cttz [inVal] [res] = unaryIntMathCodegen "cttz" inVal res

-- ============================================================================
-- Binary Floating-Point Operations (with FastMathFlag)
-- ============================================================================

mathCodegen (Atan2 fmf) [a, b] [res] = binaryFloatMathCodegen "atan2" fmf a b res
mathCodegen (Copysign fmf) [a, b] [res] = binaryFloatMathCodegen "copysign" fmf a b res
mathCodegen (Fpowi fmf) [a, b] [res] = binaryFloatMathCodegen "fpowi" fmf a b res
mathCodegen (Powf fmf) [a, b] [res] = binaryFloatMathCodegen "powf" fmf a b res

-- ============================================================================
-- Binary Integer Operations (without FastMathFlag)
-- ============================================================================

mathCodegen Ipowi [a, b] [res] = binaryIntMathCodegen "ipowi" a b res

-- ============================================================================
-- Ternary Floating-Point Operations (with FastMathFlag)
-- ============================================================================

mathCodegen (Fma fmf) [a, b, c] [res] = ternaryFloatMathCodegen "fma" fmf a b c res

-- ============================================================================
-- Special Operations (multiple results)
-- ============================================================================

mathCodegen (Sincos fmf) [(inVal, inTy)] [(sinRes, _sinTy), (cosRes, _cosTy)] = C.emitLine $ do
  C.emitBuilder $ C.emit sinRes <> ", " <> C.emit cosRes <> " = " <> "math.sincos "
  C.emitBuilder $ C.emit inVal
  fmfCodegen fmf
  C.emitBuilder $ " : " <> C.emit inTy

-- ============================================================================
-- Fallback
-- ============================================================================

mathCodegen _ _ _ = error "mathCodegen: unrecognized math intrinsic form"

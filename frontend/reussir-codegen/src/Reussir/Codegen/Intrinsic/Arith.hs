{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Intrinsic.Arith
  ( IntOFFlag (..),
    FastMathFlag (..),
    CmpIPredicate (..),
    CmpFPredicate (..),
    RoundingMode (..),
    Arith (..),
    arithCodegen,
  )
where

import Control.Monad (unless)
import Data.Bits ((.&.))
import Data.Int (Int8)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Codegen.Context qualified as C
import Reussir.Codegen.Value (TypedValue)

newtype IntOFFlag = IntOFFlag Int8
  deriving (Eq, Show)

iofIsNone :: IntOFFlag -> Bool
iofIsNone (IntOFFlag 0) = True
iofIsNone _ = False

iofNone :: IntOFFlag
iofNone = IntOFFlag 0

instance C.Emission IntOFFlag where
  emit (IntOFFlag x) = doEmit (x .&. 0b11)
    where
      doEmit :: Int8 -> TB.Builder
      doEmit 0 = "none"
      doEmit 1 = "nsw"
      doEmit 2 = "nuw"
      doEmit 3 = "nsw nuw"
      doEmit _ = undefined

newtype FastMathFlag = FastMathFlag Int8
  deriving (Eq, Show)

fmfIsNone :: FastMathFlag -> Bool
fmfIsNone (FastMathFlag 0) = True
fmfIsNone _ = False

instance C.Emission FastMathFlag where
  emit (FastMathFlag 0) = "none"
  emit (FastMathFlag 127) = "fast"
  emit (FastMathFlag m) =
    doEmit
      m
      [ (1, "reassoc"),
        (2, "nnan"),
        (4, "ninf"),
        (8, "nsz"),
        (16, "arcp"),
        (32, "contract"),
        (64, "afn")
      ]
    where
      doEmit :: Int8 -> [(Int8, TB.Builder)] -> TB.Builder
      doEmit _ [] = ""
      doEmit n ((bit, name) : xs)
        | n .&. bit /= 0 = name <> (if null xsFiltered then "" else " ") <> doEmit n xsFiltered
        | otherwise = doEmit n xs
        where
          xsFiltered = filter (\(b, _) -> n .&. b /= 0) xs

data CmpIPredicate
  = CIEq
  | CINe
  | CIUgt
  | CIUge
  | CIUlt
  | CIUle
  | CISgt
  | CISge
  | CISlt
  | CISle
  deriving (Eq, Show)

instance C.Emission CmpIPredicate where
  emit CIEq = "eq"
  emit CINe = "ne"
  emit CIUgt = "ugt"
  emit CIUge = "uge"
  emit CIUlt = "ult"
  emit CIUle = "ule"
  emit CISgt = "sgt"
  emit CISge = "sge"
  emit CISlt = "slt"
  emit CISle = "sle"

data CmpFPredicate
  = CFAlwaysFalse
  | CFOeq
  | CFOgt
  | CFOge
  | CFOlt
  | CFOle
  | CFOne
  | CFOrd
  | CFUeq
  | CFUgt
  | CFUge
  | CFUlt
  | CFUle
  | CFUne
  | CFUno
  | CFAlwaysTrue
  deriving (Eq, Show)

instance C.Emission CmpFPredicate where
  emit CFAlwaysFalse = "always_false"
  emit CFOeq = "oeq"
  emit CFOgt = "ogt"
  emit CFOge = "oge"
  emit CFOlt = "olt"
  emit CFOle = "ole"
  emit CFOne = "one"
  emit CFOrd = "ord"
  emit CFUeq = "ueq"
  emit CFUgt = "ugt"
  emit CFUge = "uge"
  emit CFUlt = "ult"
  emit CFUle = "ule"
  emit CFUne = "une"
  emit CFUno = "uno"
  emit CFAlwaysTrue = "always_true"

data RoundingMode
  = ToNearestEven
  | Downward
  | Upward
  | TowardZero
  | ToNearestAwayFromZero
  deriving (Eq, Show)

instance C.Emission RoundingMode where
  emit ToNearestEven = "rne"
  emit Downward = "rd"
  emit Upward = "ru"
  emit TowardZero = "rz"
  emit ToNearestAwayFromZero = "rna"

data Arith
  = Addf FastMathFlag
  | Addi IntOFFlag
  | AdduiExtended
  | Andi
  | Bitcast
  | Ceildivsi
  | Ceildivui
  | Cmpf CmpFPredicate FastMathFlag
  | Cmpi CmpIPredicate
  | Constant T.Text
  | Divf FastMathFlag
  | Divsi
  | Divui
  | Extf FastMathFlag
  | Extsi
  | Extui
  | Floordivsi
  | Fptosi
  | Fptoui
  | IndexCast
  | IndexCastui
  | Maximumf FastMathFlag
  | Maxnumf FastMathFlag
  | Maxsi
  | Maxui
  | Minimumf FastMathFlag
  | Minsi
  | Minui
  | Mulf FastMathFlag
  | Muli IntOFFlag
  | MulsiExtended
  | MuluiExtended
  | Negf FastMathFlag
  | Ori
  | Remf FastMathFlag
  | Remsi
  | Remui
  | ScalingExtf FastMathFlag
  | ScalingTruncf (Maybe RoundingMode) FastMathFlag
  | Select
  | Shli IntOFFlag
  | Shrsi
  | Shrui
  | Sitofp
  | Subf FastMathFlag
  | Subi IntOFFlag
  | Truncf (Maybe RoundingMode) FastMathFlag
  | Trunci IntOFFlag
  | Uitofp
  | Xori
  deriving (Eq, Show)

fmfCodegen :: FastMathFlag -> C.Codegen ()
fmfCodegen fmf = unless (fmfIsNone fmf) $ do
  C.emitBuilder $ " fastmath<" <> C.emit fmf <> ">"

iofCodegen :: IntOFFlag -> C.Codegen ()
iofCodegen iof = unless (iofIsNone iof) $ do
  C.emitBuilder $ " overflow<" <> C.emit iof <> ">"

binaryFloatArithCodegen :: TB.Builder -> FastMathFlag -> TypedValue -> TypedValue -> TypedValue -> C.Codegen ()
binaryFloatArithCodegen mnemonic fmf (vA, _) (vB, _) (resVal, resTy) = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "arith." <> mnemonic <> " "
  C.emitBuilder $ C.emit vA <> ", " <> C.emit vB
  fmfCodegen fmf
  C.emitBuilder $ " : " <> C.emit resTy

binaryIntArithCodegen :: TB.Builder -> IntOFFlag -> TypedValue -> TypedValue -> TypedValue -> C.Codegen ()
binaryIntArithCodegen mnemonic iof (vA, _) (vB, _) (resVal, resTy) = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "arith." <> mnemonic <> " "
  C.emitBuilder $ C.emit vA <> ", " <> C.emit vB
  iofCodegen iof
  C.emitBuilder $ " : " <> C.emit resTy

convertArithCodegen :: TB.Builder -> TypedValue -> TypedValue -> C.Codegen ()
convertArithCodegen mnemonic (inVal, inTy) (resVal, resTy) = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "arith." <> mnemonic <> " "
  C.emitBuilder $ C.emit inVal
  C.emitBuilder $ " : " <> C.emit inTy <> " to " <> C.emit resTy

-- | Generate MLIR assembly for arithmetic operations.
-- This function dispatches to the appropriate code generator based on the operation type.
arithCodegen :: Arith -> [TypedValue] -> [TypedValue] -> C.Codegen ()
-- ============================================================================
-- Basic Arithmetic Operations
-- ============================================================================

arithCodegen (Addf fmf) [a, b] [res] = binaryFloatArithCodegen "addf" fmf a b res
arithCodegen (Addi iof) [a, b] [res] = binaryIntArithCodegen "addi" iof a b res
arithCodegen (Subf fmf) [a, b] [res] = binaryFloatArithCodegen "subf" fmf a b res
arithCodegen (Subi iof) [a, b] [res] = binaryIntArithCodegen "subi" iof a b res
arithCodegen (Mulf fmf) [a, b] [res] = binaryFloatArithCodegen "mulf" fmf a b res
arithCodegen (Muli iof) [a, b] [res] = binaryIntArithCodegen "muli" iof a b res
arithCodegen (Divf fmf) [a, b] [res] = binaryFloatArithCodegen "divf" fmf a b res
arithCodegen Divsi [a, b] [res] = binaryIntArithCodegen "divsi" iofNone a b res
arithCodegen Divui [a, b] [res] = binaryIntArithCodegen "divui" iofNone a b res
-- Extended arithmetic operations (return two results: value and overflow flag)
arithCodegen AdduiExtended [(valA, _), (valB, _)] [(resVal, resTy), (oFlag, oFlagTy)] = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> "," <> C.emit oFlag <> " = " <> "arith.addui_extended "
  C.emitBuilder $ C.emit valA <> ", " <> C.emit valB
  C.emitBuilder $ " : " <> C.emit resTy <> ", " <> C.emit oFlagTy
arithCodegen MulsiExtended [(a, _), (b, _)] [(l, _), (h, ty)] = C.emitLine $ do
  C.emitBuilder $ C.emit l <> "," <> C.emit h <> " = " <> "arith.mulsi_extended "
  C.emitBuilder $ C.emit a <> ", " <> C.emit b
  C.emitBuilder $ " : " <> C.emit ty
arithCodegen MuluiExtended [(a, _), (b, _)] [(l, _), (h, ty)] = C.emitLine $ do
  C.emitBuilder $ C.emit l <> "," <> C.emit h <> " = " <> "arith.mului_extended "
  C.emitBuilder $ C.emit a <> ", " <> C.emit b
  C.emitBuilder $ " : " <> C.emit ty

-- Ceiling and floor division
arithCodegen Ceildivsi [a, b] [res] = binaryIntArithCodegen "ceildivsi" iofNone a b res
arithCodegen Ceildivui [a, b] [res] = binaryIntArithCodegen "ceildivui" iofNone a b res
arithCodegen Floordivsi [a, b] [res] = binaryIntArithCodegen "floordivsi" iofNone a b res
-- Remainder operations
arithCodegen (Remf fmf) [a, b] [res] = binaryFloatArithCodegen "remf" fmf a b res
arithCodegen Remsi [a, b] [res] = binaryIntArithCodegen "remsi" iofNone a b res
arithCodegen Remui [a, b] [res] = binaryIntArithCodegen "remui" iofNone a b res
-- Negation
arithCodegen (Negf fmf) [(a, _)] [(res, resTy)] = C.emitLine $ do
  C.emitBuilder $ C.emit res <> " = " <> "arith.negf "
  C.emitBuilder $ C.emit a
  fmfCodegen fmf
  C.emitBuilder $ " : " <> C.emit resTy

-- ============================================================================
-- Bitwise and Logical Operations
-- ============================================================================

arithCodegen Andi [a, b] [res] = binaryIntArithCodegen "andi" iofNone a b res
arithCodegen Ori [a, b] [res] = binaryIntArithCodegen "ori" iofNone a b res
arithCodegen Xori [a, b] [res] = binaryIntArithCodegen "xori" iofNone a b res
-- ============================================================================
-- Comparison Operations
-- ============================================================================

arithCodegen (Cmpf predicate fmf) [(a, ty), (b, _)] [(res, _)] = C.emitLine $ do
  C.emitBuilder $ C.emit res <> " = " <> "arith.cmpf " <> C.emit predicate <> ", "
  C.emitBuilder $ C.emit a <> ", " <> C.emit b
  fmfCodegen fmf
  C.emitBuilder $ " : " <> C.emit ty
arithCodegen (Cmpi predicate) [(a, ty), (b, _)] [(res, _)] = C.emitLine $ do
  C.emitBuilder $ C.emit res <> " = " <> "arith.cmpi " <> C.emit predicate <> ", "
  C.emitBuilder $ C.emit a <> ", " <> C.emit b
  C.emitBuilder $ " : " <> C.emit ty

-- ============================================================================
-- Min/Max Operations
-- ============================================================================

-- Floating-point min/max
arithCodegen (Maximumf fmf) [a, b] [res] = binaryFloatArithCodegen "maximumf" fmf a b res
arithCodegen (Maxnumf fmf) [a, b] [res] = binaryFloatArithCodegen "maxnumf" fmf a b res
arithCodegen (Minimumf fmf) [a, b] [res] = binaryFloatArithCodegen "minimumf" fmf a b res
-- Integer min/max
arithCodegen Maxsi [a, b] [res] = binaryIntArithCodegen "maxsi" iofNone a b res
arithCodegen Maxui [a, b] [res] = binaryIntArithCodegen "maxui" iofNone a b res
arithCodegen Minsi [a, b] [res] = binaryIntArithCodegen "minsi" iofNone a b res
arithCodegen Minui [a, b] [res] = binaryIntArithCodegen "minui" iofNone a b res
-- ============================================================================
-- Shift Operations
-- ============================================================================

arithCodegen (Shli iof) [a, b] [res] = binaryIntArithCodegen "shli" iof a b res
arithCodegen Shrsi [a, b] [res] = binaryIntArithCodegen "shrsi" iofNone a b res
arithCodegen Shrui [a, b] [res] = binaryIntArithCodegen "shrui" iofNone a b res
-- ============================================================================
-- Type Conversion Operations
-- ============================================================================

-- Bitcast (no-op type cast)
arithCodegen Bitcast [(valIn, tyIn)] [(valOut, tyOut)] = convertArithCodegen "bitcast" (valIn, tyIn) (valOut, tyOut)
-- Integer extension (sign-agnostic)
arithCodegen Extsi [(inVal, inTy)] [(resVal, resTy)] = convertArithCodegen "extsi" (inVal, inTy) (resVal, resTy)
arithCodegen Extui [(inVal, inTy)] [(resVal, resTy)] = convertArithCodegen "extui" (inVal, inTy) (resVal, resTy)
-- Floating-point extension
arithCodegen (Extf fmf) [(inVal, inTy)] [(resVal, resTy)] = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "arith.extf "
  C.emitBuilder $ C.emit inVal
  fmfCodegen fmf
  C.emitBuilder $ " : " <> C.emit inTy <> " to " <> C.emit resTy

-- Integer truncation
arithCodegen (Trunci iof) [(inVal, inTy)] [(resVal, resTy)] = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "arith.trunci "
  C.emitBuilder $ C.emit inVal
  iofCodegen iof
  C.emitBuilder $ " : " <> C.emit inTy <> " to " <> C.emit resTy

-- Floating-point truncation (with optional rounding mode)
arithCodegen (Truncf rm fmf) [(inVal, inTy)] [(resVal, resTy)] = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "arith.truncf "
  C.emitBuilder $ C.emit inVal
  case rm of
    Just x -> C.emitBuilder $ " " <> C.emit x
    Nothing -> pure ()
  fmfCodegen fmf
  C.emitBuilder $ " : " <> C.emit inTy <> " to " <> C.emit resTy

-- Floating-point to integer conversion
arithCodegen Fptosi [(inVal, inTy)] [(resVal, resTy)] = convertArithCodegen "fptosi" (inVal, inTy) (resVal, resTy)
arithCodegen Fptoui [(inVal, inTy)] [(resVal, resTy)] = convertArithCodegen "fptoui" (inVal, inTy) (resVal, resTy)
-- Integer to floating-point conversion
arithCodegen Sitofp [(inVal, inTy)] [(resVal, resTy)] = convertArithCodegen "sitofp" (inVal, inTy) (resVal, resTy)
arithCodegen Uitofp [(inVal, inTy)] [(resVal, resTy)] = convertArithCodegen "uitofp" (inVal, inTy) (resVal, resTy)
-- Index cast operations
arithCodegen IndexCast [(inVal, inTy)] [(resVal, resTy)] = convertArithCodegen "indexcast" (inVal, inTy) (resVal, resTy)
arithCodegen IndexCastui [(inVal, inTy)] [(resVal, resTy)] = convertArithCodegen "indexcastui" (inVal, inTy) (resVal, resTy)
-- ============================================================================
-- Scaling Operations
-- ============================================================================

-- Scaling float extension (multiply by scale factor during extension)
arithCodegen (ScalingExtf fmf) [(inVal, inTy), (sVal, sTy)] [(resVal, resTy)] = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "arith.scaling_extf "
  C.emitBuilder $ C.emit inVal <> ", " <> C.emit sVal
  fmfCodegen fmf
  C.emitBuilder $ " : " <> C.emit inTy <> ", " <> C.emit sTy <> " to " <> C.emit resTy

-- Scaling float truncation (with optional rounding mode)
arithCodegen (ScalingTruncf rm fmf) [(inVal, inTy), (sVal, sTy)] [(resVal, resTy)] = C.emitLine $ do
  C.emitBuilder $ C.emit resVal <> " = " <> "arith.scaling_truncf "
  C.emitBuilder $ C.emit inVal <> ", " <> C.emit sVal
  case rm of
    Just x -> C.emitBuilder $ " " <> C.emit x
    Nothing -> pure ()
  fmfCodegen fmf
  C.emitBuilder $ " : " <> C.emit inTy <> ", " <> C.emit sTy <> " to " <> C.emit resTy

-- ============================================================================
-- Special Operations
-- ============================================================================

-- Constant value
arithCodegen (Constant value) [] [(res, ty)] = C.emitLine $ do
  C.emitBuilder $ C.emit res <> " = arith.constant " <> TB.fromLazyText value
  C.emitBuilder $ " : " <> C.emit ty

-- Select operation (conditional)
arithCodegen Select [(cond, _), (a, _), (b, _)] [(res, resTy)] = C.emitLine $ do
  C.emitBuilder $ C.emit res <> " = " <> "arith.select "
  C.emitBuilder $ C.emit cond <> ", " <> C.emit a <> ", " <> C.emit b
  C.emitBuilder $ " : " <> C.emit resTy

-- ============================================================================
-- Fallback
-- ============================================================================

arithCodegen _ _ _ = error "arithCodegen: unrecognized arith intrinsic form"

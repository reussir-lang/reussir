{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Intrinsics (
    IntOFFlag(..),
    FastMathFlag(..),
    CmpIPredicate(..),
    CmpFPredicate(..),
    RoundingMode(..),
    Arith(..),
    Math(..),
    Intrinsic(..),
    IntrinsicCall(..),
    arithCodegen,
    intrinsicCallCodegen
)  where
import Reussir.Codegen.Context qualified as C
import Reussir.Codegen.Value (TypedValue)
import Data.Text.Lazy.Builder qualified as TB
import Data.Int (Int8)
import Data.Bits ((.&.))
import Control.Monad (unless)


newtype IntOFFlag = IntOFFlag Int8
    deriving (Eq, Show)

iofIsNone :: IntOFFlag -> Bool
iofIsNone (IntOFFlag 0) = True
iofIsNone _             = False

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
fmfIsNone _                = False

instance C.Emission FastMathFlag where
    emit (FastMathFlag 0) = "none"
    emit (FastMathFlag 127) = "fast"
    emit (FastMathFlag m) = doEmit m [
        (1, "reassoc"),
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
            doEmit n ((bit, name):xs)
                | n .&. bit /= 0 = name <> (if null xsFiltered then "" else " ") <> doEmit n xsFiltered
                | otherwise      = doEmit n xs
                where xsFiltered = filter (\(b, _) -> n .&. b /= 0) xs

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
    emit CIEq   = "eq"
    emit CINe   = "ne"
    emit CIUgt  = "ugt"
    emit CIUge  = "uge"
    emit CIUlt  = "ult"
    emit CIUle  = "ule"
    emit CISgt  = "sgt"
    emit CISge  = "sge"
    emit CISlt  = "slt"
    emit CISle  = "sle"

data CmpFPredicate 
    =  CFAlwaysFalse 
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
    emit CFOeq         = "oeq"
    emit CFOgt         = "ogt"
    emit CFOge         = "oge"
    emit CFOlt         = "olt"
    emit CFOle         = "ole"
    emit CFOne         = "one"
    emit CFOrd         = "ord"
    emit CFUeq        = "ueq"
    emit CFUgt        = "ugt"
    emit CFUge        = "uge"
    emit CFUlt        = "ult"
    emit CFUle        = "ule"
    emit CFUne        = "une"
    emit CFUno        = "uno"
    emit CFAlwaysTrue  = "always_true"

data RoundingMode 
    = ToNearestEven
    | Downward
    | Upward
    | TowardZero
    | ToNearestAwayFromZero
    deriving (Eq, Show)

instance C.Emission RoundingMode where
    emit ToNearestEven         = "rne"
    emit Downward              = "rd"
    emit Upward                = "ru"
    emit TowardZero            = "rz"
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
    | ScalingTruncf RoundingMode FastMathFlag
    | Select
    | Shli IntOFFlag
    | Shrsi
    | Shrui
    | Sitofp
    | Subf FastMathFlag
    | Subi IntOFFlag
    | Truncf RoundingMode FastMathFlag
    | Trunci IntOFFlag
    | Uitofp
    | Xori
    deriving (Eq, Show)

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

arithCodegen :: Arith -> [TypedValue] -> [TypedValue] -> C.Codegen ()
arithCodegen (Addf fmf) [a, b] [res] = binaryFloatArithCodegen "addf" fmf a b res
arithCodegen (Addi iof) [a, b] [res] = binaryIntArithCodegen "addi" iof a b res
arithCodegen (Subf fmf) [a, b] [res] = binaryFloatArithCodegen "subf" fmf a b res
arithCodegen (Subi iof) [a, b] [res] = binaryIntArithCodegen "subi" iof a b res
arithCodegen (Mulf fmf) [a, b] [res] = binaryFloatArithCodegen "mulf" fmf a b res
arithCodegen _ _ _ = error "arithCodegen: Not implemented for this intrinsic"

data Intrinsic 
    = Arith Arith
    | Math Math
    deriving (Eq, Show)

data IntrinsicCall
    = IntrinsicCall {
        target :: Intrinsic,
        args :: [TypedValue],
        results :: [TypedValue]
    } deriving (Eq, Show)

intrinsicCallCodegen :: IntrinsicCall -> C.Codegen ()
intrinsicCallCodegen (IntrinsicCall (Arith arith) args rets) =
    arithCodegen arith args rets
intrinsicCallCodegen _ = error "intrinsicCallCodegen: Not implemented for this intrinsic"
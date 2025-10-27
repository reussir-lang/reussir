{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Intrinsics where
import Data.Int (Int8)
import Reussir.Codegen.Context qualified as C
import Data.Bits (Bits(..))
import Data.Text.Lazy.Builder qualified as TB

data IntOFFlag = IONone | IONsw | IONuw
    deriving (Eq, Show)

instance C.Emission IntOFFlag where
    emit IONone = "none"
    emit IONsw  = "nsw"
    emit IONuw  = "nuw"

data FastMathFlag
    = FMNone
    | FMReassoc
    | FMNnan
    | FMNinf
    | FMNsz
    | FMArcp
    | FMContract
    | FMAfn
    | FMFast
    deriving (Eq, Show)

instance C.Emission FastMathFlag where
    emit FMNone      = "none"
    emit FMReassoc   = "reassoc"
    emit FMNnan      = "nnan"
    emit FMNinf      = "ninf"
    emit FMNsz       = "nsz"
    emit FMArcp      = "arcp"
    emit FMContract  = "contract"
    emit FMAfn       = "afn"
    emit FMFast      = "fast"

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

data Arith 
    = AddI IntOFFlag
    | SubI IntOFFlag
    | MulI IntOFFlag
    | Addf FastMathFlag
    | Subf FastMathFlag
    | Cmpi CmpIPredicate
    | Cmpf CmpFPredicate
    | Divsi
    | Divui
    | Muli IntOFFlag
    | Remsi
    | Remui
    deriving (Eq, Show)


data Intrinsic 
    = Arith Arith
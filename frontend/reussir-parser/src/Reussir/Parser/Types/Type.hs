module Reussir.Parser.Types.Type where

import Data.Int (Int8)
import Data.List (intercalate)
import Data.Text qualified as T

newtype Identifier = Identifier {unIdentifier :: T.Text}

data Path = Path
    { pathBasename :: Identifier
    , pathSegments :: [Identifier]
    }

instance Show Path where
    show (Path base segs) =
        intercalate "::" (map show (segs ++ [base]))

instance Show Identifier where
    show (Identifier name) = '$' : T.unpack name

data IntegralType
    = Signed {-# UNPACK #-} !Int8
    | Unsigned {-# UNPACK #-} !Int8

instance Show IntegralType where
    show (Signed bits) = "i" ++ show bits
    show (Unsigned bits) = "u" ++ show bits

data FloatingPointType
    = FP16
    | FP32
    | FP64
    | FP128

instance Show FloatingPointType where
    show FP16 = "f16"
    show FP32 = "f32"
    show FP64 = "f64"
    show FP128 = "f128"

data Type
    = TypeExpr Path [Type]
    | TypeIntegral IntegralType
    | TypeFP FloatingPointType
    | TypeBool
    | TypeString
    | TypeUnit

instance Show Type where
    show (TypeExpr path []) = show path
    show (TypeExpr path args) =
        show path
            ++ "<"
            ++ intercalate ", " (map show args)
            ++ ">"
    show (TypeIntegral it) = show it
    show (TypeFP fpt) = show fpt
    show TypeBool = "bool"
    show TypeString = "string"
    show TypeUnit = "unit"

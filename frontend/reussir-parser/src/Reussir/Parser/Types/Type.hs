module Reussir.Parser.Types.Type where

import Data.Int (Int8)
import Data.List (intercalate)
import Reussir.Parser.Types.Lexer (Path (..))

data IntegralType
    = Signed {-# UNPACK #-} !Int8
    | Unsigned {-# UNPACK #-} !Int8

instance Show IntegralType where
    show (Signed bits) = "i" ++ show bits
    show (Unsigned bits) = "u" ++ show bits

data FloatingPointType
    = IEEEFloat !Int8
    | BFloat16
    | Float8 -- TODO

instance Show FloatingPointType where
    show (IEEEFloat bits) = "f" ++ show bits
    show BFloat16 = "bfloat16"
    show Float8 = "float8"

data Type
    = TypeExpr Path [Type]
    | TypeIntegral IntegralType
    | TypeFP FloatingPointType
    | TypeBool
    | TypeStr
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
    show TypeStr = "str"
    show TypeUnit = "unit"

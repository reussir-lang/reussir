module Reussir.Core.Data.Full.Type where

import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Codegen.Type.Data (Capability)

import Data.IntMap qualified as IntMap

import Reussir.Core.Data.FP (FloatingPointType)
import Reussir.Core.Data.Integral (IntegralType)

import Reussir.Core.Data.Semi.Type qualified as Semi

data Type
    = -- | User-defined type with a path and optional type parameters
      TypeRecord Symbol
    | -- | Integral type (signed or unsigned)
      TypeIntegral IntegralType
    | -- | Floating-point type
      TypeFP FloatingPointType
    | -- | Boolean type
      TypeBool
    | -- | String type
      TypeStr
    | -- | Unit type (similar to void)
      TypeUnit
    | -- | Closure type
      TypeClosure [Type] Type
    | -- | Bottom type
      TypeBottom
    | -- | Type of a nullable region
      TypeNullable Type
    | TypeRc Type Capability
    deriving (Show, Eq)

type GenericMap = IntMap.IntMap Semi.Type

module Reussir.Core.Data.Integral where

import Data.Hashable (Hashable (..))
import Data.Int (Int16)

{- | Represents integral types with a specific bit width.
Integral types can be either signed or unsigned.
-}
data IntegralType
    = -- | Signed integer with specified bit width (e.g., i8, i16, i32, i64)
      Signed {-# UNPACK #-} !Int16
    | -- | Unsigned integer with specified bit width (e.g., u8, u16, u32, u64)
      Unsigned {-# UNPACK #-} !Int16
    deriving (Eq)

instance Hashable IntegralType where
    hashWithSalt salt (Signed bits) =
        salt `hashWithSalt` True `hashWithSalt` bits
    hashWithSalt salt (Unsigned bits) =
        salt `hashWithSalt` False `hashWithSalt` bits

instance Show IntegralType where
    show (Signed bits) = "i" ++ show bits
    show (Unsigned bits) = "u" ++ show bits

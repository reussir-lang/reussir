module Reussir.Core2.Types.FP where

import Data.Hashable (Hashable (..))
import Data.Int (Int16)

{- | Represents floating-point types.
Supports IEEE floating-point formats as well as specialized formats like BFloat16 and Float8.
-}
data FloatingPointType
    = -- | IEEE floating-point with specified bit width (e.g., f16, f32, f64)
      IEEEFloat !Int16
    | -- | Brain Floating Point format (16-bit) used in ML applications
      BFloat16
    | -- | 8-bit floating-point format (specification pending)
      Float8
    deriving (Eq)

instance Show FloatingPointType where
    show (IEEEFloat bits) = "f" ++ show bits
    show BFloat16 = "bfloat16"
    show Float8 = "float8"

instance Hashable FloatingPointType where
    hashWithSalt salt (IEEEFloat bits) =
        salt `hashWithSalt` (0 :: Int) `hashWithSalt` bits
    hashWithSalt salt BFloat16 =
        salt `hashWithSalt` (1 :: Int)
    hashWithSalt salt Float8 =
        salt `hashWithSalt` (2 :: Int)

{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Intrinsics
  ( IntOFFlag (..),
    FastMathFlag (..),
    CmpIPredicate (..),
    CmpFPredicate (..),
    RoundingMode (..),
    Arith (..),
    Math (..),
    Intrinsic (..),
    IntrinsicCall (..),
    arithCodegen,
    intrinsicCallCodegen,
  )
where

import Reussir.Codegen.Context qualified as C
import Reussir.Codegen.Intrinsic.Arith
  ( Arith (..),
    CmpFPredicate (..),
    CmpIPredicate (..),
    FastMathFlag (..),
    IntOFFlag (..),
    RoundingMode (..),
    arithCodegen,
  )
import Reussir.Codegen.Intrinsic.Math (Math (..))
import Reussir.Codegen.Value (TypedValue)

data Intrinsic
  = Arith Arith
  | Math Math
  deriving (Eq, Show)

data IntrinsicCall
  = IntrinsicCall
  { target :: Intrinsic,
    args :: [TypedValue],
    results :: [TypedValue]
  }
  deriving (Eq, Show)

intrinsicCallCodegen :: IntrinsicCall -> C.Codegen ()
intrinsicCallCodegen (IntrinsicCall (Arith arith) args rets) =
  arithCodegen arith args rets
intrinsicCallCodegen _ = error "intrinsicCallCodegen: Not implemented for this intrinsic"

{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Intrinsic.Math
  ( Math (..),
  )
where

import Reussir.Codegen.Intrinsic.Arith (FastMathFlag)

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

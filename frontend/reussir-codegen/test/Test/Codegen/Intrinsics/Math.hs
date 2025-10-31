{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.Intrinsics.Math
  ( mathTests,
  )
where

import Control.Monad.State.Strict qualified as S
import Data.Int (Int64)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Builder qualified as TB
import Reussir.Bridge qualified as B
import Reussir.Codegen.Context qualified as C
import Reussir.Codegen.Intrinsics qualified as I
import Reussir.Codegen.Type qualified as TT
import Reussir.Codegen.Value qualified as V
import Test.Tasty
import Test.Tasty.HUnit

runEmptyCodegenAsText :: C.Codegen () -> IO T.Text
runEmptyCodegenAsText codegen = do
  let spec = C.TargetSpec "module" "output.mlir" B.OptDefault B.OutputObject B.LogInfo
  (_, finalCtx) <- S.runStateT codegen (C.emptyContext spec)
  return $ TB.toLazyText (C.builder finalCtx)

runCodegenForICall :: I.IntrinsicCall -> IO T.Text
runCodegenForICall icall =
  runEmptyCodegenAsText (I.intrinsicCallCodegen icall)

primitiveI32 :: TT.Type
primitiveI32 = TT.TypePrim (TT.PrimInt TT.PrimInt32)

primitiveF64 :: TT.Type
primitiveF64 = TT.TypePrim (TT.PrimFloat TT.PrimFloat64)

-- Helper functions for math operations
absf64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
absf64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Absf flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveF64)]

absi32 :: Int64 -> Int64 -> I.IntrinsicCall
absi32 dst src =
  I.IntrinsicCall
    (I.Math I.Absi)
    [(V.Value src, primitiveI32)]
    [(V.Value dst, primitiveI32)]

sin64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
sin64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Sin flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveF64)]

cos64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
cos64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Cos flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveF64)]

sqrt64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
sqrt64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Sqrt flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveF64)]

exp64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
exp64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Exp flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveF64)]

log264 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
log264 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Log2 flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveF64)]

floor64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
floor64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Floor flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveF64)]

ceil64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
ceil64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Ceil flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveF64)]

atan64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
atan64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Atan flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveF64)]

tanh64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
tanh64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Tanh flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveF64)]

powf64 :: Int64 -> Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
powf64 dst src1 src2 flag =
  I.IntrinsicCall
    (I.Math (I.Powf flag))
    [(V.Value src1, primitiveF64), (V.Value src2, primitiveF64)]
    [(V.Value dst, primitiveF64)]

atan264 :: Int64 -> Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
atan264 dst src1 src2 flag =
  I.IntrinsicCall
    (I.Math (I.Atan2 flag))
    [(V.Value src1, primitiveF64), (V.Value src2, primitiveF64)]
    [(V.Value dst, primitiveF64)]

copysign64 :: Int64 -> Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
copysign64 dst src1 src2 flag =
  I.IntrinsicCall
    (I.Math (I.Copysign flag))
    [(V.Value src1, primitiveF64), (V.Value src2, primitiveF64)]
    [(V.Value dst, primitiveF64)]

fma64 :: Int64 -> Int64 -> Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
fma64 dst src1 src2 src3 flag =
  I.IntrinsicCall
    (I.Math (I.Fma flag))
    [(V.Value src1, primitiveF64), (V.Value src2, primitiveF64), (V.Value src3, primitiveF64)]
    [(V.Value dst, primitiveF64)]

fpowi64 :: Int64 -> Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
fpowi64 dst src1 src2 flag =
  I.IntrinsicCall
    (I.Math (I.Fpowi flag))
    [(V.Value src1, primitiveF64), (V.Value src2, primitiveI32)]
    [(V.Value dst, primitiveF64)]

ipowi32 :: Int64 -> Int64 -> Int64 -> I.IntrinsicCall
ipowi32 dst src1 src2 =
  I.IntrinsicCall
    (I.Math I.Ipowi)
    [(V.Value src1, primitiveI32), (V.Value src2, primitiveI32)]
    [(V.Value dst, primitiveI32)]

ctlz32 :: Int64 -> Int64 -> I.IntrinsicCall
ctlz32 dst src =
  I.IntrinsicCall
    (I.Math I.Ctlz)
    [(V.Value src, primitiveI32)]
    [(V.Value dst, primitiveI32)]

ctpop32 :: Int64 -> Int64 -> I.IntrinsicCall
ctpop32 dst src =
  I.IntrinsicCall
    (I.Math I.Ctpop)
    [(V.Value src, primitiveI32)]
    [(V.Value dst, primitiveI32)]

cttz32 :: Int64 -> Int64 -> I.IntrinsicCall
cttz32 dst src =
  I.IntrinsicCall
    (I.Math I.Cttz)
    [(V.Value src, primitiveI32)]
    [(V.Value dst, primitiveI32)]

isfinite64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
isfinite64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Isfinite flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveI32)]

isinf64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
isinf64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Isinf flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveI32)]

isnan64 :: Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
isnan64 dst src flag =
  I.IntrinsicCall
    (I.Math (I.Isnan flag))
    [(V.Value src, primitiveF64)]
    [(V.Value dst, primitiveI32)]

sincos64 :: Int64 -> Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
sincos64 sinDst cosDst src flag =
  I.IntrinsicCall
    (I.Math (I.Sincos flag))
    [(V.Value src, primitiveF64)]
    [(V.Value sinDst, primitiveF64), (V.Value cosDst, primitiveF64)]

(@@?=) :: (Eq a, Show a, HasCallStack) => IO a -> a -> Assertion
action @@?= expected = action >>= (@?= expected)

mathTests :: TestTree
mathTests =
  testGroup
    "Math.Intrinsic.Codegen"
    [ testCase "ABSF Codegen without FastMath" $
        runCodegenForICall (absf64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.absf %1 : f64\n",
      testCase "ABSF Codegen with FastMath" $
        runCodegenForICall (absf64 3 1 $ I.FastMathFlag 127) @@?= "%3 = math.absf %1 fastmath<fast> : f64\n",
      testCase "ABSF Codegen with Reassoc and NInf" $
        runCodegenForICall (absf64 3 1 $ I.FastMathFlag 5)
          @@?= "%3 = math.absf %1 fastmath<reassoc ninf> : f64\n",
      testCase "ABSI Codegen" $
        runCodegenForICall (absi32 3 1) @@?= "%3 = math.absi %1 : i32\n",
      testCase "SIN Codegen" $
        runCodegenForICall (sin64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.sin %1 : f64\n",
      testCase "SIN Codegen with FastMath" $
        runCodegenForICall (sin64 3 1 $ I.FastMathFlag 127) @@?= "%3 = math.sin %1 fastmath<fast> : f64\n",
      testCase "COS Codegen" $
        runCodegenForICall (cos64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.cos %1 : f64\n",
      testCase "SQRT Codegen" $
        runCodegenForICall (sqrt64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.sqrt %1 : f64\n",
      testCase "SQRT Codegen with FastMath" $
        runCodegenForICall (sqrt64 3 1 $ I.FastMathFlag 127) @@?= "%3 = math.sqrt %1 fastmath<fast> : f64\n",
      testCase "EXP Codegen" $
        runCodegenForICall (exp64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.exp %1 : f64\n",
      testCase "LOG2 Codegen" $
        runCodegenForICall (log264 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.log2 %1 : f64\n",
      testCase "FLOOR Codegen" $
        runCodegenForICall (floor64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.floor %1 : f64\n",
      testCase "CEIL Codegen" $
        runCodegenForICall (ceil64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.ceil %1 : f64\n",
      testCase "ATAN Codegen" $
        runCodegenForICall (atan64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.atan %1 : f64\n",
      testCase "TANH Codegen" $
        runCodegenForICall (tanh64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.tanh %1 : f64\n",
      testCase "POWF Codegen" $
        runCodegenForICall (powf64 3 1 2 $ I.FastMathFlag 0) @@?= "%3 = math.powf %1, %2 : f64\n",
      testCase "POWF Codegen with FastMath" $
        runCodegenForICall (powf64 3 1 2 $ I.FastMathFlag 127) @@?= "%3 = math.powf %1, %2 fastmath<fast> : f64\n",
      testCase "ATAN2 Codegen" $
        runCodegenForICall (atan264 3 1 2 $ I.FastMathFlag 0) @@?= "%3 = math.atan2 %1, %2 : f64\n",
      testCase "COPYSIGN Codegen" $
        runCodegenForICall (copysign64 3 1 2 $ I.FastMathFlag 0) @@?= "%3 = math.copysign %1, %2 : f64\n",
      testCase "FMA Codegen" $
        runCodegenForICall (fma64 3 1 2 4 $ I.FastMathFlag 0) @@?= "%3 = math.fma %1, %2, %4 : f64\n",
      testCase "FMA Codegen with FastMath" $
        runCodegenForICall (fma64 3 1 2 4 $ I.FastMathFlag 127) @@?= "%3 = math.fma %1, %2, %4 fastmath<fast> : f64\n",
      testCase "FPOWI Codegen" $
        runCodegenForICall (fpowi64 3 1 2 $ I.FastMathFlag 0) @@?= "%3 = math.fpowi %1, %2 : f64, i32\n",
      testCase "IPOWI Codegen" $
        runCodegenForICall (ipowi32 3 1 2) @@?= "%3 = math.ipowi %1, %2 : i32\n",
      testCase "CTLZ Codegen" $
        runCodegenForICall (ctlz32 3 1) @@?= "%3 = math.ctlz %1 : i32\n",
      testCase "CTPOP Codegen" $
        runCodegenForICall (ctpop32 3 1) @@?= "%3 = math.ctpop %1 : i32\n",
      testCase "CTTZ Codegen" $
        runCodegenForICall (cttz32 3 1) @@?= "%3 = math.cttz %1 : i32\n",
      testCase "ISFINITE Codegen" $
        runCodegenForICall (isfinite64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.isfinite %1 : i32\n",
      testCase "ISINF Codegen" $
        runCodegenForICall (isinf64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.isinf %1 : i32\n",
      testCase "ISNAN Codegen" $
        runCodegenForICall (isnan64 3 1 $ I.FastMathFlag 0) @@?= "%3 = math.isnan %1 : i32\n",
      testCase "SINCOS Codegen without FastMath" $
        runCodegenForICall (sincos64 3 4 1 $ I.FastMathFlag 0) @@?= "%3, %4 = math.sincos %1 : f64\n",
      testCase "SINCOS Codegen with FastMath" $
        runCodegenForICall (sincos64 3 4 1 $ I.FastMathFlag 127) @@?= "%3, %4 = math.sincos %1 fastmath<fast> : f64\n",
      testCase "SINCOS Codegen with Reassoc and NInf" $
        runCodegenForICall (sincos64 3 4 1 $ I.FastMathFlag 5)
          @@?= "%3, %4 = math.sincos %1 fastmath<reassoc ninf> : f64\n"
    ]


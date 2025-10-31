{-# LANGUAGE OverloadedStrings #-}

module Test.Codegen.Intrinsics.Arith
  ( arithTests,
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

primitiveI64 :: TT.Type
primitiveI64 = TT.TypePrim (TT.PrimInt TT.PrimInt64)

primitiveF32 :: TT.Type
primitiveF32 = TT.TypePrim (TT.PrimFloat TT.PrimFloat32)

primitiveF64 :: TT.Type
primitiveF64 = TT.TypePrim (TT.PrimFloat TT.PrimFloat64)

primitiveF128 :: TT.Type
primitiveF128 = TT.TypePrim (TT.PrimFloat TT.PrimFloat128)

-- Helper functions for arith operations
addi32 :: Int64 -> Int64 -> Int64 -> I.IntOFFlag -> I.IntrinsicCall
addi32 dst src1 src2 flag =
  I.IntrinsicCall
    (I.Arith (I.Addi flag))
    [(V.Value src1, primitiveI32), (V.Value src2, primitiveI32)]
    [(V.Value dst, primitiveI32)]

addf64 :: Int64 -> Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
addf64 dst src1 src2 flag =
  I.IntrinsicCall
    (I.Arith (I.Addf flag))
    [(V.Value src1, primitiveF64), (V.Value src2, primitiveF64)]
    [(V.Value dst, primitiveF64)]

cmpf128 :: I.CmpFPredicate -> I.FastMathFlag -> Int64 -> Int64 -> Int64 -> I.IntrinsicCall
cmpf128 predicate flag src1 src2 dst =
  I.IntrinsicCall
    (I.Arith (I.Cmpf predicate flag))
    [(V.Value src1, primitiveF128), (V.Value src2, primitiveF128)]
    [(V.Value dst, primitiveF128)]

constant :: T.Text -> Int64 -> I.IntrinsicCall
constant value dst =
  I.IntrinsicCall
    (I.Arith (I.Constant value))
    []
    [(V.Value dst, primitiveF128)]

subi32 :: Int64 -> Int64 -> Int64 -> I.IntOFFlag -> I.IntrinsicCall
subi32 dst src1 src2 flag =
  I.IntrinsicCall
    (I.Arith (I.Subi flag))
    [(V.Value src1, primitiveI32), (V.Value src2, primitiveI32)]
    [(V.Value dst, primitiveI32)]

muli32 :: Int64 -> Int64 -> Int64 -> I.IntOFFlag -> I.IntrinsicCall
muli32 dst src1 src2 flag =
  I.IntrinsicCall
    (I.Arith (I.Muli flag))
    [(V.Value src1, primitiveI32), (V.Value src2, primitiveI32)]
    [(V.Value dst, primitiveI32)]

divf64 :: Int64 -> Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
divf64 dst src1 src2 flag =
  I.IntrinsicCall
    (I.Arith (I.Divf flag))
    [(V.Value src1, primitiveF64), (V.Value src2, primitiveF64)]
    [(V.Value dst, primitiveF64)]

andi32 :: Int64 -> Int64 -> Int64 -> I.IntrinsicCall
andi32 dst src1 src2 =
  I.IntrinsicCall
    (I.Arith I.Andi)
    [(V.Value src1, primitiveI32), (V.Value src2, primitiveI32)]
    [(V.Value dst, primitiveI32)]

ori32 :: Int64 -> Int64 -> Int64 -> I.IntrinsicCall
ori32 dst src1 src2 =
  I.IntrinsicCall
    (I.Arith I.Ori)
    [(V.Value src1, primitiveI32), (V.Value src2, primitiveI32)]
    [(V.Value dst, primitiveI32)]

xori32 :: Int64 -> Int64 -> Int64 -> I.IntrinsicCall
xori32 dst src1 src2 =
  I.IntrinsicCall
    (I.Arith I.Xori)
    [(V.Value src1, primitiveI32), (V.Value src2, primitiveI32)]
    [(V.Value dst, primitiveI32)]

bitcast :: Int64 -> Int64 -> TT.Type -> TT.Type -> I.IntrinsicCall
bitcast dst src fromTy toTy =
  I.IntrinsicCall
    (I.Arith I.Bitcast)
    [(V.Value src, fromTy)]
    [(V.Value dst, toTy)]

extsi :: Int64 -> Int64 -> TT.Type -> TT.Type -> I.IntrinsicCall
extsi dst src fromTy toTy =
  I.IntrinsicCall
    (I.Arith I.Extsi)
    [(V.Value src, fromTy)]
    [(V.Value dst, toTy)]

extui :: Int64 -> Int64 -> TT.Type -> TT.Type -> I.IntrinsicCall
extui dst src fromTy toTy =
  I.IntrinsicCall
    (I.Arith I.Extui)
    [(V.Value src, fromTy)]
    [(V.Value dst, toTy)]

extf :: Int64 -> Int64 -> TT.Type -> TT.Type -> I.FastMathFlag -> I.IntrinsicCall
extf dst src fromTy toTy flag =
  I.IntrinsicCall
    (I.Arith (I.Extf flag))
    [(V.Value src, fromTy)]
    [(V.Value dst, toTy)]

trunci :: Int64 -> Int64 -> TT.Type -> TT.Type -> I.IntOFFlag -> I.IntrinsicCall
trunci dst src fromTy toTy flag =
  I.IntrinsicCall
    (I.Arith (I.Trunci flag))
    [(V.Value src, fromTy)]
    [(V.Value dst, toTy)]

fptosi :: Int64 -> Int64 -> TT.Type -> TT.Type -> I.IntrinsicCall
fptosi dst src fromTy toTy =
  I.IntrinsicCall
    (I.Arith I.Fptosi)
    [(V.Value src, fromTy)]
    [(V.Value dst, toTy)]

sitofp :: Int64 -> Int64 -> TT.Type -> TT.Type -> I.IntrinsicCall
sitofp dst src fromTy toTy =
  I.IntrinsicCall
    (I.Arith I.Sitofp)
    [(V.Value src, fromTy)]
    [(V.Value dst, toTy)]

maxsi32 :: Int64 -> Int64 -> Int64 -> I.IntrinsicCall
maxsi32 dst src1 src2 =
  I.IntrinsicCall
    (I.Arith I.Maxsi)
    [(V.Value src1, primitiveI32), (V.Value src2, primitiveI32)]
    [(V.Value dst, primitiveI32)]

maximumf64 :: Int64 -> Int64 -> Int64 -> I.FastMathFlag -> I.IntrinsicCall
maximumf64 dst src1 src2 flag =
  I.IntrinsicCall
    (I.Arith (I.Maximumf flag))
    [(V.Value src1, primitiveF64), (V.Value src2, primitiveF64)]
    [(V.Value dst, primitiveF64)]

shli32 :: Int64 -> Int64 -> Int64 -> I.IntOFFlag -> I.IntrinsicCall
shli32 dst src1 src2 flag =
  I.IntrinsicCall
    (I.Arith (I.Shli flag))
    [(V.Value src1, primitiveI32), (V.Value src2, primitiveI32)]
    [(V.Value dst, primitiveI32)]

cmpi :: I.CmpIPredicate -> Int64 -> Int64 -> Int64 -> I.IntrinsicCall
cmpi predicate src1 src2 dst =
  I.IntrinsicCall
    (I.Arith (I.Cmpi predicate))
    [(V.Value src1, primitiveI32), (V.Value src2, primitiveI32)]
    [(V.Value dst, primitiveI32)]

(@@?=) :: (Eq a, Show a, HasCallStack) => IO a -> a -> Assertion
action @@?= expected = action >>= (@?= expected)

arithTests :: TestTree
arithTests =
  testGroup
    "Arith.Intrinsic.Codegen"
    [ testCase "ADDI Codegen" $
        runCodegenForICall (addi32 3 1 2 $ I.IntOFFlag 0) @@?= "%3 = arith.addi %1, %2 : i32\n",
      testCase "ADDI Codegen with NSW" $
        runCodegenForICall (addi32 3 1 2 $ I.IntOFFlag 1) @@?= "%3 = arith.addi %1, %2 overflow<nsw> : i32\n",
      testCase "ADDI Codegen with NSW NUW" $
        runCodegenForICall (addi32 3 1 2 $ I.IntOFFlag 3) @@?= "%3 = arith.addi %1, %2 overflow<nsw nuw> : i32\n",
      testCase "ADDF Codegen" $
        runCodegenForICall (addf64 3 1 2 $ I.FastMathFlag 0) @@?= "%3 = arith.addf %1, %2 : f64\n",
      testCase "ADDF Codegen with FastMath" $
        runCodegenForICall (addf64 3 1 2 $ I.FastMathFlag 127) @@?= "%3 = arith.addf %1, %2 fastmath<fast> : f64\n",
      testCase "ADDF Codegen with Reassoc and NInf" $
        runCodegenForICall (addf64 3 1 2 $ I.FastMathFlag 5)
          @@?= "%3 = arith.addf %1, %2 fastmath<reassoc ninf> : f64\n",
      testCase
        "CMPF Codegen oeq"
        $ runCodegenForICall (cmpf128 I.CFOeq (I.FastMathFlag 0) 1 2 3)
          @@?= "%3 = arith.cmpf oeq, %1, %2 : f128\n",
      testCase
        "Constant Codegen"
        $ runCodegenForICall (constant "1.0" 0) @@?= "%0 = arith.constant 1.0 : f128\n",
      -- More arithmetic operations
      testCase "SUBI Codegen" $
        runCodegenForICall (subi32 3 1 2 $ I.IntOFFlag 0) @@?= "%3 = arith.subi %1, %2 : i32\n",
      testCase "MULI Codegen" $
        runCodegenForICall (muli32 3 1 2 $ I.IntOFFlag 2) @@?= "%3 = arith.muli %1, %2 overflow<nuw> : i32\n",
      testCase "DIVF Codegen" $
        runCodegenForICall (divf64 3 1 2 $ I.FastMathFlag 0) @@?= "%3 = arith.divf %1, %2 : f64\n",
      -- Bitwise operations
      testCase "ANDI Codegen" $
        runCodegenForICall (andi32 3 1 2) @@?= "%3 = arith.andi %1, %2 : i32\n",
      testCase "ORI Codegen" $
        runCodegenForICall (ori32 3 1 2) @@?= "%3 = arith.ori %1, %2 : i32\n",
      testCase "XORI Codegen" $
        runCodegenForICall (xori32 3 1 2) @@?= "%3 = arith.xori %1, %2 : i32\n",
      -- Conversion operations
      testCase "Bitcast Codegen" $
        runCodegenForICall (bitcast 3 1 primitiveI32 primitiveF32) @@?= "%3 = arith.bitcast %1 : i32 to f32\n",
      testCase "EXTSI Codegen" $
        runCodegenForICall (extsi 3 1 primitiveI32 primitiveI64) @@?= "%3 = arith.extsi %1 : i32 to i64\n",
      testCase "EXTUI Codegen" $
        runCodegenForICall (extui 3 1 primitiveI32 primitiveI64) @@?= "%3 = arith.extui %1 : i32 to i64\n",
      testCase "EXTF Codegen" $
        runCodegenForICall (extf 3 1 primitiveF32 primitiveF64 $ I.FastMathFlag 0) @@?= "%3 = arith.extf %1 : f32 to f64\n",
      testCase "EXTF Codegen with FastMath" $
        runCodegenForICall (extf 3 1 primitiveF32 primitiveF64 $ I.FastMathFlag 127) @@?= "%3 = arith.extf %1 fastmath<fast> : f32 to f64\n",
      testCase "TRUNCI Codegen" $
        runCodegenForICall (trunci 3 1 primitiveI64 primitiveI32 $ I.IntOFFlag 0) @@?= "%3 = arith.trunci %1 : i64 to i32\n",
      testCase "TRUNCI Codegen with NSW" $
        runCodegenForICall (trunci 3 1 primitiveI64 primitiveI32 $ I.IntOFFlag 1) @@?= "%3 = arith.trunci %1 overflow<nsw> : i64 to i32\n",
      testCase "FPTOSI Codegen" $
        runCodegenForICall (fptosi 3 1 primitiveF64 primitiveI32) @@?= "%3 = arith.fptosi %1 : f64 to i32\n",
      testCase "SITOFP Codegen" $
        runCodegenForICall (sitofp 3 1 primitiveI32 primitiveF32) @@?= "%3 = arith.sitofp %1 : i32 to f32\n",
      -- Min/Max operations
      testCase "MAXSI Codegen" $
        runCodegenForICall (maxsi32 3 1 2) @@?= "%3 = arith.maxsi %1, %2 : i32\n",
      testCase "MAXIMUMF Codegen" $
        runCodegenForICall (maximumf64 3 1 2 $ I.FastMathFlag 0) @@?= "%3 = arith.maximumf %1, %2 : f64\n",
      -- Shift operations
      testCase "SHLI Codegen" $
        runCodegenForICall (shli32 3 1 2 $ I.IntOFFlag 0) @@?= "%3 = arith.shli %1, %2 : i32\n",
      testCase "SHLI Codegen with NUW" $
        runCodegenForICall (shli32 3 1 2 $ I.IntOFFlag 2) @@?= "%3 = arith.shli %1, %2 overflow<nuw> : i32\n",
      -- Integer comparison
      testCase "CMPI Codegen eq" $
        runCodegenForICall (cmpi I.CIEq 1 2 3) @@?= "%3 = arith.cmpi eq, %1, %2 : i32\n",
      testCase "CMPI Codegen slt" $
        runCodegenForICall (cmpi I.CISlt 1 2 3) @@?= "%3 = arith.cmpi slt, %1, %2 : i32\n"
    ]

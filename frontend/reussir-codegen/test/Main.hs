{-# LANGUAGE OverloadedStrings #-}

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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

runEmptyCodegenAsText :: C.Codegen () -> T.Text
runEmptyCodegenAsText codegen =
  let spec = C.TargetSpec "module" "output.mlir" B.OptDefault B.OutputObject B.LogInfo
      (_, finalCtx) = S.runState codegen (C.emptyContext spec)
   in TB.toLazyText (C.builder finalCtx)

runCodegenForICall :: I.IntrinsicCall -> T.Text
runCodegenForICall icall =
  runEmptyCodegenAsText (I.intrinsicCallCodegen icall)

primitiveI32 :: TT.Type
primitiveI32 = TT.TypePrimitive (TT.PrimInt TT.PrimInt32)

primitiveF64 :: TT.Type
primitiveF64 = TT.TypePrimitive (TT.PrimFloat TT.PrimFloat64)

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

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "ADDI Codegen" $
        runCodegenForICall (addi32 3 1 2 $ I.IntOFFlag 0) @?= "%3 = arith.addi %1, %2 : i32\n",
      testCase "ADDI Codegen with NSW" $
        runCodegenForICall (addi32 3 1 2 $ I.IntOFFlag 1) @?= "%3 = arith.addi %1, %2 overflow<nsw> : i32\n",
      testCase "ADDI Codegen with NSW NUW" $
        runCodegenForICall (addi32 3 1 2 $ I.IntOFFlag 3) @?= "%3 = arith.addi %1, %2 overflow<nsw nuw> : i32\n",
      testCase "ADDF Codegen" $
        runCodegenForICall (addf64 3 1 2 $ I.FastMathFlag 0) @?= "%3 = arith.addf %1, %2 : f64\n",
      testCase "ADDF Codegen with FastMath" $
        runCodegenForICall (addf64 3 1 2 $ I.FastMathFlag 127) @?= "%3 = arith.addf %1, %2 fastmath<fast> : f64\n",
      testCase "ADDF Codegen with Reassoc and NInf" $
        runCodegenForICall (addf64 3 1 2 $ I.FastMathFlag 5) @?= "%3 = arith.addf %1, %2 fastmath<reassoc ninf> : f64\n"
    ]

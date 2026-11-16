// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir | %FileCheck %s

// CHECK-LABEL: define i64 @scalable(
// CHECK-SAME: <vscale x 16 x i1> %[[M:.*]])
// CHECK: call i64 @llvm.experimental.cttz.elts.i64.nxv16i1(<vscale x 16 x i1> %[[M]], i1 false)
func.func @scalable(%m: vector<[16]xi1>) -> i64 {
  %n = reussir.vector.cttz_elts %m : vector<[16]xi1> -> i64
  return %n : i64
}

// CHECK-LABEL: define i32 @fixed(
// CHECK-SAME: <8 x i1> %[[M:.*]])
// CHECK: call i32 @llvm.experimental.cttz.elts.i32.v8i1(<8 x i1> %[[M]], i1 false)
func.func @fixed(%m: vector<8xi1>) -> i32 {
  %n = reussir.vector.cttz_elts %m : vector<8xi1> -> i32
  return %n : i32
}

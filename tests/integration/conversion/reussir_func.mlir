// RUN: %reussir-opt %s --reussir-lowering-basic-ops | %FileCheck %s

module @test {
  reussir.func @simple_func(%arg0: i64) -> i64 {
    reussir.return %arg0 : i64
  }
  // CHECK-LABEL: llvm.func @simple_func(%arg0: i64) -> i64
  // CHECK: llvm.return %arg0 : i64

  reussir.func private @private_func() {
    reussir.return
  }
  // CHECK-LABEL: llvm.func private @private_func()
  // CHECK: llvm.return

  reussir.func @external_func() -> i64
  // CHECK-LABEL: llvm.func @external_func() -> i64

  reussir.func private @external_private_func() -> i64
  // CHECK-LABEL: llvm.func @external_private_func() -> i64

  reussir.func @caller() {
    %c0 = arith.constant 0 : i64
    %0 = reussir.call @simple_func(%c0) : (i64) -> i64
    reussir.return
  }
  // CHECK-LABEL: llvm.func @caller()
  // CHECK: llvm.call @simple_func(%{{.*}}) : (i64) -> i64

  reussir.func @multi_return(%arg0: i64, %arg1: i64) -> (i64, i64) {
    reussir.return %arg0, %arg1 : i64, i64
  }
  // CHECK-LABEL: llvm.func @multi_return(%arg0: i64, %arg1: i64) -> !llvm.struct<(i64, i64)>
  // CHECK: %[[UNDEF:.*]] = llvm.mlir.undef : !llvm.struct<(i64, i64)>
  // CHECK: %[[INS1:.*]] = llvm.insertvalue %arg0, %[[UNDEF]][0]
  // CHECK: %[[INS2:.*]] = llvm.insertvalue %arg1, %[[INS1]][1]
  // CHECK: llvm.return %[[INS2]]
}

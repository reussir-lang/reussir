// RUN: %reussir-opt %s --reussir-lowering-basic-ops | %FileCheck %s

module @test_abi attributes { llvm.target_triple = "x86_64-pc-windows-msvc" } {
  reussir.func @large_struct_ret() -> !reussir.record<compound "Big" {i64, i64, i64}> {
      %c0 = arith.constant 0 : i64
      %0 = reussir.record.compound(%c0, %c0, %c0 : i64, i64, i64) : !reussir.record<compound "Big" {i64, i64, i64}>
      reussir.return %0 : !reussir.record<compound "Big" {i64, i64, i64}>
  }
  // CHECK-LABEL: llvm.func @large_struct_ret(%arg0: !llvm.ptr {llvm.sret = !llvm.struct<"Big", (i64, i64, i64)>})
  // CHECK: llvm.store %{{.*}}, %arg0
  // CHECK: llvm.return

  reussir.func @caller_abi() {
      %0 = reussir.call @large_struct_ret() : () -> !reussir.record<compound "Big" {i64, i64, i64}>
      reussir.return
  }
  // CHECK-LABEL: llvm.func @caller_abi()
  // CHECK: %[[ALLOCA:.*]] = llvm.alloca
  // CHECK: llvm.call @large_struct_ret(%[[ALLOCA]])
  // CHECK: llvm.load %[[ALLOCA]]
}

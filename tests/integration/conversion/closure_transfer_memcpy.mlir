// RUN: %reussir-opt %s --reussir-lowering-basic-ops --convert-to-llvm --reconcile-unrealized-casts | %FileCheck %s --implicit-check-not=memcpy.inline

// Small closure copies retain a constant size without forcing the inline-only
// intrinsic, which TPDE cannot compile. Large closures keep cursor-based copies.
module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>> } {
  func.func @small(%src: !reussir.ref<!reussir.closure_box<i64> unspecified>, %dst: !reussir.ref<!reussir.closure_box<i64> unspecified>) {
    reussir.closure.transfer %src to %dst : !reussir.ref<!reussir.closure_box<i64> unspecified> to !reussir.ref<!reussir.closure_box<i64> unspecified>
    return
  }
  // CHECK-LABEL: llvm.func @small
  // CHECK: %[[SIZE:.*]] = llvm.mlir.constant(16 : i64) : i64
  // CHECK: "llvm.intr.memcpy"({{.*}}, %[[SIZE]]) <{isVolatile = false}>

  func.func @large(%src: !reussir.ref<!reussir.closure_box<i64, i64, i64, i64, i64> unspecified>, %dst: !reussir.ref<!reussir.closure_box<i64, i64, i64, i64, i64> unspecified>) {
    reussir.closure.transfer %src to %dst : !reussir.ref<!reussir.closure_box<i64, i64, i64, i64, i64> unspecified> to !reussir.ref<!reussir.closure_box<i64, i64, i64, i64, i64> unspecified>
    return
  }
  // CHECK-LABEL: llvm.func @large
  // CHECK: %[[WORD:.*]] = llvm.mlir.constant(8 : i64) : i64
  // CHECK-NEXT: %[[LENGTH:.*]] = llvm.sub {{.*}}, %[[WORD]]
  // CHECK: "llvm.intr.memcpy"({{.*}}, %[[LENGTH]]) <{isVolatile = false}>
}

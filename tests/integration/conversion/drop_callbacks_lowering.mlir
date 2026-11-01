// RUN: %reussir-opt %s --reussir-convert-to-llvm --verify-each | %FileCheck %s
// CHECK-DAG: llvm.func @__reussir_drop_join_or_run(!llvm.ptr, !llvm.ptr)
// CHECK-DAG: llvm.func @__reussir_drop_run(!llvm.ptr, !llvm.ptr)
// CHECK-DAG: llvm.func @__reussir_drop_defer(!llvm.ptr, !llvm.ptr)
// CHECK-DAG: llvm.func @__reussir_drop_defer_indexed(!llvm.ptr, i64, !llvm.ptr)
func.func private @release(%arg: !reussir.ref<i64>) {
  return
}
func.func private @step(%arg: !reussir.ref<i64>, %index: index) {
  return
}
func.func @callbacks(%arg: !reussir.ref<i64>) {
  %zero = arith.constant 0 : index
  // CHECK: llvm.mlir.addressof @release
  // CHECK: llvm.call @__reussir_drop_run
  reussir.drop.join @release(%arg : !reussir.ref<i64>)
  reussir.drop.run @release(%arg : !reussir.ref<i64>)
  // CHECK: llvm.call @__reussir_drop_defer
  reussir.drop.defer @release(%arg : !reussir.ref<i64>)
  // CHECK: llvm.mlir.addressof @step
  // CHECK: llvm.call @__reussir_drop_defer_indexed
  reussir.drop.defer_indexed @step(%arg : !reussir.ref<i64>, %zero)
  return
}

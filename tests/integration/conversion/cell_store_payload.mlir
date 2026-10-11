// RUN: %reussir-opt %s --reussir-lowering-basic-ops | %reussir-translate --mlir-to-llvmir | %FileCheck %s --implicit-check-not=llvm.invariant.start --implicit-check-not=alloca
// RUN: %reussir-opt %s --convert-to-llvm | %reussir-translate --mlir-to-llvmir | %FileCheck %s --implicit-check-not=llvm.invariant.start --implicit-check-not=alloca

// The storage bridge supports all LLVM-storable payloads, not just nullable
// pointers and strings. Representative pointer and aggregate payloads below
// do not implement MemRefElementTypeInterface. The cell handle is already
// borrowed/guarded by the higher-level lowering before this operation.

// CHECK-LABEL: define void @reference(
// CHECK: store ptr
func.func @reference(%value: !reussir.ref<i64>, %cell: !reussir.rc<!reussir.cell<!reussir.ref<i64>>>) {
  reussir.cell.store_payload(%value : !reussir.ref<i64>, %cell : !reussir.rc<!reussir.cell<!reussir.ref<i64>>>)
  return
}

// CHECK-LABEL: define void @token(
// CHECK: store ptr
func.func @token(%value: !reussir.token<align : 8, size : 8>, %cell: !reussir.rc<!reussir.cell<!reussir.token<align : 8, size : 8>>>) {
  reussir.cell.store_payload(%value : !reussir.token<align : 8, size : 8>, %cell : !reussir.rc<!reussir.cell<!reussir.token<align : 8, size : 8>>>)
  return
}

// CHECK-LABEL: define void @region(
// CHECK: store ptr
func.func @region(%value: !reussir.region, %cell: !reussir.rc<!reussir.cell<!reussir.region>>) {
  reussir.cell.store_payload(%value : !reussir.region, %cell : !reussir.rc<!reussir.cell<!reussir.region>>)
  return
}

// CHECK-LABEL: define void @hole(
// CHECK: store ptr
func.func @hole(%value: !reussir.hole<i64>, %cell: !reussir.rc<!reussir.cell<!reussir.hole<i64>>>) {
  reussir.cell.store_payload(%value : !reussir.hole<i64>, %cell : !reussir.rc<!reussir.cell<!reussir.hole<i64>>>)
  return
}

// CHECK-LABEL: define void @array(
// CHECK: store [4 x i64]
func.func @array(%value: !reussir.array<4 x i64>, %cell: !reussir.rc<!reussir.cell<!reussir.array<4 x i64>>>) {
  reussir.cell.store_payload(%value : !reussir.array<4 x i64>, %cell : !reussir.rc<!reussir.cell<!reussir.array<4 x i64>>>)
  return
}

// CHECK-LABEL: define void @nested_cell(
// CHECK: store { i64, i1 }
func.func @nested_cell(%value: !reussir.cell<i64 exclusive>, %cell: !reussir.rc<!reussir.cell<!reussir.cell<i64 exclusive>>>) {
  reussir.cell.store_payload(%value : !reussir.cell<i64 exclusive>, %cell : !reussir.rc<!reussir.cell<!reussir.cell<i64 exclusive>>>)
  return
}

// CHECK-LABEL: define void @context(
// CHECK: store { ptr, ptr }
func.func @context(%value: !reussir.cctx<!reussir.rc<i64>>, %cell: !reussir.rc<!reussir.cell<!reussir.cctx<!reussir.rc<i64>>>>) {
  reussir.cell.store_payload(%value : !reussir.cctx<!reussir.rc<i64>>, %cell : !reussir.rc<!reussir.cell<!reussir.cctx<!reussir.rc<i64>>>>)
  return
}

// RUN: %reussir-opt %s --reussir-convert-to-std | %FileCheck %s --implicit-check-not=' field' --implicit-check-not='reussir.ref.spilled' --implicit-check-not='memref<!reussir.nullable' --implicit-check-not='memref<!reussir.str'
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-convert-to-std,reussir-acquire-drop-expansion{expand-decrement=1 outline-record=1},func.func(reussir-token-reuse),reussir-convert-to-std,convert-scf-to-cf,reussir-lowering-basic-ops,convert-to-llvm,reconcile-unrealized-casts,canonicalize,cse)' | %reussir-translate --mlir-to-llvmir | %FileCheck %s --check-prefix=LLVM

// Plain and exclusive Cells accept payloads that are not memref elements.
// Storing these values must preserve their representation without acquiring
// another owner, manufacturing a regional field reference, or spilling to an
// invariant temporary that could be overwritten in a loop.

!nullable = !reussir.nullable<!reussir.rc<i64>>
!string = !reussir.str<global>

// CHECK-LABEL: func.func @create_nullable
// CHECK: reussir.cell.store_payload
// CHECK: return
// LLVM-LABEL: define {{.*}}@create_nullable(
// LLVM: store
func.func @create_nullable(%value: !nullable) -> !reussir.rc<!reussir.cell<!nullable>> {
  %cell = reussir.cell.create value(%value : !nullable) : !reussir.rc<!reussir.cell<!nullable>>
  return %cell : !reussir.rc<!reussir.cell<!nullable>>
}

// CHECK-LABEL: func.func @set_nullable
// CHECK: reussir.cell.store_payload
// CHECK: return
// LLVM-LABEL: define {{.*}}@set_nullable(
// LLVM: store
func.func @set_nullable(%value: !nullable, %cell: !reussir.rc<!reussir.cell<!nullable>>) {
  reussir.cell.set(%value : !nullable, %cell : !reussir.rc<!reussir.cell<!nullable>>)
  return
}

// CHECK-LABEL: func.func @create_nullable_exclusive
// CHECK: reussir.cell.store_payload
// CHECK: return
// LLVM-LABEL: define {{.*}}@create_nullable_exclusive(
// LLVM: store
func.func @create_nullable_exclusive(%value: !nullable) -> !reussir.rc<!reussir.cell<!nullable exclusive>> {
  %cell = reussir.cell.create value(%value : !nullable) : !reussir.rc<!reussir.cell<!nullable exclusive>>
  return %cell : !reussir.rc<!reussir.cell<!nullable exclusive>>
}

// CHECK-LABEL: func.func @set_nullable_exclusive
// CHECK: reussir.cell.store_payload
// CHECK: return
// LLVM-LABEL: define {{.*}}@set_nullable_exclusive(
// LLVM: store
func.func @set_nullable_exclusive(%value: !nullable, %cell: !reussir.rc<!reussir.cell<!nullable exclusive>>) {
  reussir.cell.set(%value : !nullable, %cell : !reussir.rc<!reussir.cell<!nullable exclusive>>)
  return
}

// CHECK-LABEL: func.func @rmw_nullable_exclusive
// CHECK: reussir.cell.store_payload
// CHECK: return
// LLVM-LABEL: define {{.*}}@rmw_nullable_exclusive(
// LLVM: store
func.func @rmw_nullable_exclusive(%value: !nullable, %cell: !reussir.rc<!reussir.cell<!nullable exclusive>>) -> !nullable {
  %old = reussir.cell.rmw(%cell : !reussir.rc<!reussir.cell<!nullable exclusive>>) -> !nullable {
    ^bb0(%current: !nullable):
      reussir.cell.yield(%value : !nullable) output(%current : !nullable)
  }
  return %old : !nullable
}

// CHECK-LABEL: func.func @create_string
// CHECK: reussir.cell.store_payload
// CHECK: return
// LLVM-LABEL: define {{.*}}@create_string(
// LLVM: store
func.func @create_string(%value: !string) -> !reussir.rc<!reussir.cell<!string>> {
  %cell = reussir.cell.create value(%value : !string) : !reussir.rc<!reussir.cell<!string>>
  return %cell : !reussir.rc<!reussir.cell<!string>>
}

// CHECK-LABEL: func.func @set_string
// CHECK: reussir.cell.store_payload
// CHECK: return
// LLVM-LABEL: define {{.*}}@set_string(
// LLVM: store
func.func @set_string(%value: !string, %cell: !reussir.rc<!reussir.cell<!string>>) {
  reussir.cell.set(%value : !string, %cell : !reussir.rc<!reussir.cell<!string>>)
  return
}

// CHECK-LABEL: func.func @create_string_exclusive
// CHECK: reussir.cell.store_payload
// CHECK: return
// LLVM-LABEL: define {{.*}}@create_string_exclusive(
// LLVM: store
func.func @create_string_exclusive(%value: !string) -> !reussir.rc<!reussir.cell<!string exclusive>> {
  %cell = reussir.cell.create value(%value : !string) : !reussir.rc<!reussir.cell<!string exclusive>>
  return %cell : !reussir.rc<!reussir.cell<!string exclusive>>
}

// CHECK-LABEL: func.func @set_string_exclusive
// CHECK: reussir.cell.store_payload
// CHECK: return
// LLVM-LABEL: define {{.*}}@set_string_exclusive(
// LLVM: store
func.func @set_string_exclusive(%value: !string, %cell: !reussir.rc<!reussir.cell<!string exclusive>>) {
  reussir.cell.set(%value : !string, %cell : !reussir.rc<!reussir.cell<!string exclusive>>)
  return
}

// CHECK-LABEL: func.func @rmw_string_exclusive
// CHECK: reussir.cell.store_payload
// CHECK: return
// LLVM-LABEL: define {{.*}}@rmw_string_exclusive(
// LLVM: store
func.func @rmw_string_exclusive(%value: !string, %cell: !reussir.rc<!reussir.cell<!string exclusive>>) -> !string {
  %old = reussir.cell.rmw(%cell : !reussir.rc<!reussir.cell<!string exclusive>>) -> !string {
    ^bb0(%current: !string):
      reussir.cell.yield(%value : !string) output(%current : !string)
  }
  return %old : !string
}

// The store can repeat without creating an invariant stack spill.
// CHECK-LABEL: func.func @set_string_loop
// CHECK: scf.for
// CHECK: reussir.cell.store_payload
// LLVM-LABEL: define {{.*}}@set_string_loop(
// LLVM-NOT: alloca
// LLVM-NOT: invariant.start
// LLVM: store { ptr, i64 }
// LLVM-NOT: alloca
// LLVM-NOT: invariant.start
// LLVM: ret void
func.func @set_string_loop(%value: !string, %cell: !reussir.rc<!reussir.cell<!string>>, %count: index) {
  %zero = arith.constant 0 : index
  %one = arith.constant 1 : index
  scf.for %i = %zero to %count step %one {
    reussir.cell.set(%value : !string, %cell : !reussir.rc<!reussir.cell<!string>>)
  }
  return
}

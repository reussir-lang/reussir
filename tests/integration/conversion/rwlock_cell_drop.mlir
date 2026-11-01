// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion,reussir-acquire-drop-expansion)' | %FileCheck %s --check-prefix=DROP
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-convert-to-std,reussir-acquire-drop-expansion{expand-decrement=1 outline-record=1},func.func(reussir-token-reuse),reussir-convert-to-std,convert-scf-to-cf,reussir-lowering-basic-ops,convert-to-llvm,reconcile-unrealized-casts,canonicalize,cse)' | %reussir-translate --mlir-to-llvmir | %FileCheck %s --check-prefix=LLVM

!inner = !reussir.rc<i64 atomic>
!rwlock_rc = !reussir.rc<!reussir.cell<!inner rwlock> atomic>

module {
  // The write critical section passes its protected slot to a release helper.
  // A leading-slot projection would address the lock header.
  // DROP-LABEL: func.func private @__reussir_locked_drop.forward(
  // DROP: %[[INNER:.+]] = reussir.ref.load(
  // DROP: reussir.rc.dec(%[[INNER]] : !reussir.rc<i64 atomic>)
  // DROP-LABEL: func.func @rwlock_cell_drop
  // DROP: reussir.rc.fetch_sub
  // DROP-NOT: reussir.ref.project
  // DROP: %[[VIEW:.+]] = reussir.ref.to_memref{{.*}} : memref<!sync.rwlock<!reussir.rc<i64 atomic>>>
  // DROP: sync.rwlock.write_critical_section %[[VIEW]]
  // DROP: ^bb0(%[[PAYLOAD:.+]]: memref<!reussir.rc<i64 atomic>>):
  // DROP: %[[SLOT:.+]] = reussir.ref.from_memref(%[[PAYLOAD]]
  // DROP: call @__reussir_locked_drop.forward(%[[SLOT]])
  // DROP-NOT: reussir.ref.project

  // The helper decrements under the lock and synchronously finishes dead
  // payload cleanup; the cell owner is freed after unlock.
  // LLVM-LABEL: define private void @__reussir_locked_drop.forward(ptr
  // LLVM: %[[INNER:.+]] = load ptr, ptr %0
  // LLVM: atomicrmw sub ptr %[[INNER]], i32 1
  // LLVM: call void @__reussir_deallocate(ptr %[[INNER]], i64 8, i64 16)
  // LLVM-LABEL: define void @rwlock_cell_drop(ptr
  // LLVM: atomicrmw sub ptr %{{.+}}, i32 1
  // LLVM: %[[CELL:.+]] = getelementptr { i32, { { i32, i32 }, ptr } }, ptr %{{.+}}, i32 0, i32 1
  // LLVM: %[[HEADER:.+]] = getelementptr { { i32, i32 }, ptr }, ptr %[[CELL]], i32 0, i32 0
  // LLVM: %[[STATE:.+]] = getelementptr { i32, i32 }, ptr %[[HEADER]], i32 0, i32 0
  // LLVM: cmpxchg ptr %[[STATE]], i32 0, i32 1073741823
  // LLVM: %[[SLOT:.+]] = getelementptr { { i32, i32 }, ptr }, ptr %[[CELL]], i32 0, i32 1
  // LLVM: call void @__reussir_locked_drop.forward(ptr %[[SLOT]])
  // LLVM: atomicrmw sub ptr %{{.+}}, i32 1073741823 release
  // LLVM: call void @__reussir_deallocate(ptr %{{.+}}, i64 8, i64 24)
  func.func @rwlock_cell_drop(%cell: !rwlock_rc) {
    %t = reussir.rc.dec (%cell : !rwlock_rc) : !reussir.nullable<!reussir.token<align: 8, size: 24>>
    reussir.token.free (%t : !reussir.nullable<!reussir.token<align: 8, size: 24>>)
    return
  }
}

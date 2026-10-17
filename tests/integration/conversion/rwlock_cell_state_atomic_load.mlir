// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-convert-to-std,reussir-acquire-drop-expansion{expand-decrement=1 outline-record=1},func.func(reussir-token-reuse),reussir-convert-to-std,convert-scf-to-cf,reussir-lowering-basic-ops,convert-to-llvm,reconcile-unrealized-casts,canonicalize,cse)' | %reussir-translate --mlir-to-llvmir | %FileCheck %s

// The rwlock fast paths read the lock state word before the cmpxchg that
// takes the lock. Other threads update that word with cmpxchg/atomicrmw, so
// the read must be an atomic load: a plain load is a data race under the LLVM
// memory model (ThreadSanitizer reports it). Monotonic is enough, because the
// acquire cmpxchg is what synchronizes with the previous unlock.

!rwlock_i64 = !reussir.rc<!reussir.cell<i64 rwlock> atomic>

module {
  // Write path: the state word must read 0 (unlocked) before the cmpxchg
  // installs the writer mask.
  // CHECK-LABEL: define i64 @write_section
  // CHECK: %[[STATE:.+]] = load atomic i32, ptr %[[WORD:.+]] monotonic, align 4
  // CHECK: icmp eq i32 %[[STATE]], 0
  // CHECK: cmpxchg ptr %[[WORD]], i32 0, i32 1073741823 acquire monotonic
  // CHECK: ret i64
  func.func @write_section(%delta: i64, %cell: !rwlock_i64) -> i64 {
    %old = reussir.cell.rmw(%cell : !rwlock_i64) -> i64 {
      ^bb0(%current: i64):
        %next = arith.addi %current, %delta : i64
        reussir.cell.yield(%next : i64) output(%current : i64)
    }
    return %old : i64
  }

  // Read path: the loaded state is the cmpxchg's expected value, and one more
  // reader is the desired value.
  // CHECK-LABEL: define i64 @read_section
  // CHECK: %[[STATE:.+]] = load atomic i32, ptr %[[WORD:.+]] monotonic, align 4
  // CHECK: %[[NEXT:.+]] = add i32 %[[STATE]], 1
  // CHECK: cmpxchg ptr %[[WORD]], i32 %[[STATE]], i32 %[[NEXT]] acquire monotonic
  // CHECK: ret i64
  func.func @read_section(%cell: !rwlock_i64) -> i64 {
    %value = reussir.cell.rdlock(%cell : !rwlock_i64) -> i64 {
      ^bb0(%current: i64):
        reussir.scf.yield %current : i64
    }
    return %value : i64
  }
}

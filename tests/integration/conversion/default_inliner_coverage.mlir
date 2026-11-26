// RUN: %reussir-opt %s --reussir-default-inliner='max-callee-ops=6' | %FileCheck %s --check-prefix=INLINE --implicit-check-not=reussir.inliner_home
// RUN: %reussir-opt %s --reussir-default-inliner='max-callee-ops=5' | %FileCheck %s --check-prefix=CAPPED --implicit-check-not=reussir.inliner_home

// The helper has 6 operations and 7 more for its two counters. The size limit
// does not count the counters, so a limit of 6 inlines the helper. Each caller
// gets the increments of the helper, so its counts add up over the two callers.
// The inliner removes the helper, and its name stays for the increments.
module {
  // INLINE: llvm.mlir.global private constant @__profn_helper
  // CAPPED: llvm.mlir.global private constant @__profn_helper
  llvm.mlir.global private constant @__profn_helper("helper") {addr_space = 0 : i32}

  // INLINE-NOT: func.func private @helper
  // CAPPED: func.func private @helper
  func.func private @helper(%x: i32, %c: i1) -> i32 {
    %name = llvm.mlir.addressof @__profn_helper : !llvm.ptr
    %hash = llvm.mlir.constant(1234 : i64) : i64
    %num = llvm.mlir.constant(2 : i32) : i32
    %c0 = llvm.mlir.constant(0 : i32) : i32
    llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %c0) : (!llvm.ptr, i64, i32, i32) -> ()
    %r = scf.if %c -> (i32) {
      %c1 = llvm.mlir.constant(1 : i32) : i32
      llvm.call_intrinsic "llvm.instrprof.increment"(%name, %hash, %num, %c1) : (!llvm.ptr, i64, i32, i32) -> ()
      %one = arith.constant 1 : i32
      %y = arith.addi %x, %one : i32
      scf.yield %y : i32
    } else {
      scf.yield %x : i32
    }
    return %r : i32
  }

  // INLINE-LABEL: func.func @left
  // INLINE-DAG: %[[NAME:.*]] = llvm.mlir.addressof @__profn_helper
  // INLINE-DAG: %[[HASH:.*]] = llvm.mlir.constant(1234 : i64)
  // INLINE-DAG: %[[ZERO:.*]] = llvm.mlir.constant(0 : i32)
  // INLINE-DAG: %[[ONE:.*]] = llvm.mlir.constant(1 : i32)
  // INLINE: llvm.call_intrinsic "llvm.instrprof.increment"(%[[NAME]], %[[HASH]], %{{.*}}, %[[ZERO]])
  // INLINE: scf.if
  // INLINE: llvm.call_intrinsic "llvm.instrprof.increment"(%[[NAME]], %[[HASH]], %{{.*}}, %[[ONE]])
  // INLINE-NOT: call @helper
  // INLINE: return
  // CAPPED-LABEL: func.func @left
  // CAPPED: call @helper
  func.func @left(%x: i32, %c: i1) -> i32 {
    %r = func.call @helper(%x, %c) : (i32, i1) -> i32
    return %r : i32
  }

  // INLINE-LABEL: func.func @right
  // INLINE-DAG: %[[NAME:.*]] = llvm.mlir.addressof @__profn_helper
  // INLINE-DAG: %[[HASH:.*]] = llvm.mlir.constant(1234 : i64)
  // INLINE-DAG: %[[ZERO:.*]] = llvm.mlir.constant(0 : i32)
  // INLINE-DAG: %[[ONE:.*]] = llvm.mlir.constant(1 : i32)
  // INLINE: llvm.call_intrinsic "llvm.instrprof.increment"(%[[NAME]], %[[HASH]], %{{.*}}, %[[ZERO]])
  // INLINE: scf.if
  // INLINE: llvm.call_intrinsic "llvm.instrprof.increment"(%[[NAME]], %[[HASH]], %{{.*}}, %[[ONE]])
  // INLINE-NOT: call @helper
  // INLINE: return
  // CAPPED-LABEL: func.func @right
  // CAPPED: call @helper
  func.func @right(%x: i32, %c: i1) -> i32 {
    %r = func.call @helper(%x, %c) : (i32, i1) -> i32
    return %r : i32
  }
}

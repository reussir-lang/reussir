// RUN: %reussir-opt %s --reussir-default-inliner='max-callee-ops=5' | %FileCheck %s --check-prefix=PLAIN
// RUN: %reussir-opt %s --reussir-instrument-coverage --reussir-default-inliner='max-callee-ops=5' | %FileCheck %s --check-prefix=INLINE
// RUN: %reussir-opt %s --reussir-instrument-coverage --reussir-default-inliner='max-callee-ops=4' | %FileCheck %s --check-prefix=CAPPED

// The helper has 5 operations. With its three counters, the inliner makes the
// same decision at each size limit as without them. Each caller gets the
// increments of the helper, so the counts of the helper add up over the two
// callers. The inliner removes the helper, and its name stays.

// INLINE: llvm.mlir.global private constant @__profn_helper("helper")
// INLINE-NOT: func.func private @helper
// PLAIN-NOT: func.func private @helper
// CAPPED: func.func private @helper
func.func private @helper(%c: i1, %x: i64) -> i64 {
  %r = scf.if %c -> (i64) {
    %a = arith.addi %x, %x : i64 loc("a.rr":3:5 to 3:10)
    scf.yield %a : i64 loc("a.rr":3:5 to 3:10)
  } else {
    scf.yield %x : i64 loc("a.rr":5:5 to 5:6)
  } loc("a.rr":2:3 to 6:4)
  return %r : i64 loc("a.rr":2:3 to 6:4)
} loc("a.rr":1:1 to 7:2)

// INLINE-LABEL: func.func @left(
// INLINE: llvm.mlir.addressof @__profn_left
// INLINE: llvm.mlir.addressof @__profn_helper
// INLINE: llvm.call_intrinsic "llvm.instrprof.increment"
// INLINE: scf.if
// INLINE: llvm.call_intrinsic "llvm.instrprof.increment"
// INLINE: } else {
// INLINE: llvm.call_intrinsic "llvm.instrprof.increment"
// INLINE-NOT: call @helper
// INLINE: return
// PLAIN-LABEL: func.func @left(
// PLAIN-NOT: call @helper
// PLAIN: return
// CAPPED-LABEL: func.func @left(
// CAPPED: call @helper
func.func @left(%c: i1, %x: i64) -> i64 {
  %r = func.call @helper(%c, %x) : (i1, i64) -> i64 loc("a.rr":9:3 to 9:18)
  return %r : i64 loc("a.rr":9:3 to 9:18)
} loc("a.rr":8:1 to 10:2)

// INLINE-LABEL: func.func @right(
// INLINE: llvm.mlir.addressof @__profn_right
// INLINE: llvm.mlir.addressof @__profn_helper
// INLINE: llvm.call_intrinsic "llvm.instrprof.increment"
// INLINE: scf.if
// INLINE: llvm.call_intrinsic "llvm.instrprof.increment"
// INLINE: } else {
// INLINE: llvm.call_intrinsic "llvm.instrprof.increment"
// INLINE-NOT: call @helper
// INLINE: return
// PLAIN-LABEL: func.func @right(
// PLAIN-NOT: call @helper
// PLAIN: return
// CAPPED-LABEL: func.func @right(
// CAPPED: call @helper
func.func @right(%c: i1, %x: i64) -> i64 {
  %r = func.call @helper(%c, %x) : (i1, i64) -> i64 loc("a.rr":12:3 to 12:18)
  return %r : i64 loc("a.rr":12:3 to 12:18)
} loc("a.rr":11:1 to 13:2)

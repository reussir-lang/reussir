// RUN: %reussir-opt %s --reussir-instrument-coverage --mlir-print-local-scope | %FileCheck %s

// The frontend emits a `while` loop as `scf.while`. The condition region
// runs one time more than the body region, so each region has its own
// counter. The increment is the first op of the entry block of the region.
// The walk is in pre-order, so an outer loop gets its counters first.

module {
  // CHECK-LABEL: func.func @nested(
  // CHECK-SAME: counters = 5 : i32
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("loops.rr":1:29 to 3:2)}, {counter = 1 : i32, span = loc("loops.rr":2:20 to :25)}, {counter = 2 : i32, span = loc("loops.rr":2:27 to :63)}, {counter = 3 : i32, span = loc("loops.rr":2:44 to :50)}, {counter = 4 : i32, span = loc("loops.rr":2:52 to :61)}]}
  // CHECK: scf.while
  // CHECK: llvm.mlir.constant(1 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.cmpi ult
  // CHECK: } do {
  // CHECK: llvm.mlir.constant(2 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: scf.while
  // CHECK: llvm.mlir.constant(3 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.cmpi eq
  // CHECK: } do {
  // CHECK: llvm.mlir.constant(4 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.constant 1
  func.func @nested(%n: i64) -> i64 {
    %zero = arith.constant 0 : i64 loc("loops.rr":2:16 to 2:17)
    %r = scf.while (%i = %zero) : (i64) -> i64 {
      %lt = arith.cmpi ult, %i, %n : i64 loc("loops.rr":2:20 to 2:25)
      scf.condition(%lt) %i : i64 loc("loops.rr":2:20 to 2:25)
    } do {
    ^bb0(%i: i64):
      %inner = scf.while (%j = %i) : (i64) -> i64 {
        %eq = arith.cmpi eq, %j, %i : i64 loc("loops.rr":2:44 to 2:50)
        scf.condition(%eq) %j : i64 loc("loops.rr":2:44 to 2:50)
      } do {
      ^bb0(%j: i64):
        %one = arith.constant 1 : i64 loc("loops.rr":2:58 to 2:59)
        %next = arith.addi %j, %one : i64 loc("loops.rr":2:54 to 2:59)
        scf.yield %next : i64 loc("loops.rr":2:52 to 2:61)
      } loc("loops.rr":2:29 to 2:61)
      scf.yield %inner : i64 loc("loops.rr":2:27 to 2:63)
    } loc("loops.rr":2:5 to 2:63)
    return %r : i64 loc("loops.rr":1:29 to 3:2)
  } loc("loops.rr":1:29 to 3:2)

  // A loop with no state has no block arguments. A body with only the yield
  // gets a counter too, because the yield has the span of the body.
  // CHECK-LABEL: func.func @drain(
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("loops.rr":5:31 to 7:2)}, {counter = 1 : i32, span = loc("loops.rr":6:15 to :32)}, {counter = 2 : i32, span = loc("loops.rr":6:34 to :38)}]}
  // CHECK: scf.while : () -> () {
  // CHECK: llvm.mlir.constant(1 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: func.call @poll
  // CHECK: } do {
  // CHECK: llvm.mlir.constant(2 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: scf.yield
  func.func private @poll() -> i1

  func.func @drain() {
    scf.while : () -> () {
      %c = func.call @poll() : () -> i1 loc("loops.rr":6:15 to 6:32)
      scf.condition(%c) loc("loops.rr":6:15 to 6:32)
    } do {
      scf.yield loc("loops.rr":6:34 to 6:38)
    } loc("loops.rr":6:5 to 6:38)
    return loc("loops.rr":5:31 to 7:2)
  } loc("loops.rr":5:31 to 7:2)

  // A region of a loop with the span of the loop gets no counter. The loop
  // of a builtin, such as `array::fold`, has only the span of the call. A
  // counter there counts the elements, not the calls.
  // CHECK-LABEL: func.func @sum(
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("loops.rr":9:35 to 11:2)}]}
  // CHECK: scf.for
  // CHECK-NOT: llvm.call_intrinsic
  // CHECK: scf.yield
  func.func @sum(%m: memref<8xf64>) -> f64 {
    %c0 = arith.constant 0 : index loc("loops.rr":10:5 to 10:42)
    %c1 = arith.constant 1 : index loc("loops.rr":10:5 to 10:42)
    %c8 = arith.constant 8 : index loc("loops.rr":10:5 to 10:42)
    %init = arith.constant 0.0 : f64 loc("loops.rr":10:25 to 10:28)
    %r = scf.for %iv = %c0 to %c8 step %c1 iter_args(%acc = %init) -> (f64) {
      %x = memref.load %m[%iv] : memref<8xf64> loc("loops.rr":10:5 to 10:42)
      %s = arith.addf %acc, %x : f64 loc("loops.rr":10:5 to 10:42)
      scf.yield %s : f64 loc("loops.rr":10:5 to 10:42)
    } loc("loops.rr":10:5 to :42)
    return %r : f64 loc("loops.rr":9:35 to 11:2)
  } loc("loops.rr":9:35 to 11:2)

  // A loop body with a span of its own gets a counter.
  // CHECK-LABEL: func.func @fill(
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("loops.rr":12:40 to 14:2)}, {counter = 1 : i32, span = loc("loops.rr":13:24 to :36)}]}
  // CHECK: scf.for
  // CHECK: llvm.mlir.constant(1 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: memref.store
  func.func @fill(%m: memref<?xi64>, %v: i64) {
    %c0 = arith.constant 0 : index loc("loops.rr":13:5 to 13:38)
    %c1 = arith.constant 1 : index loc("loops.rr":13:5 to 13:38)
    %ub = memref.dim %m, %c0 : memref<?xi64> loc("loops.rr":13:5 to 13:38)
    scf.for %iv = %c0 to %ub step %c1 {
      memref.store %v, %m[%iv] : memref<?xi64> loc("loops.rr":13:24 to 13:36)
    } loc("loops.rr":13:5 to 13:38)
    return loc("loops.rr":12:40 to 14:2)
  } loc("loops.rr":12:40 to 14:2)
}

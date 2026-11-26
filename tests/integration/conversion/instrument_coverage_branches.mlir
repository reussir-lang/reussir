// RUN: %reussir-opt %s --reussir-instrument-coverage --mlir-print-local-scope | %FileCheck %s
// RUN: %reussir-opt %s --reussir-instrument-coverage --mlir-print-debuginfo \
// RUN:   | %reussir-opt --reussir-instrument-coverage --mlir-print-local-scope \
// RUN:   | %FileCheck %s

// The IR is as the frontend makes it for this source:
//
//   1 | pub fn pick(x: i64) -> i64 {
//   2 |     if x > 0 {
//   3 |         x + 1
//   4 |     } else {
//   5 |         x
//   6 |     }
//   7 | }
//   8 | pub fn both(a: i64, b: i64) -> bool { a > 0 && b > 0 }
//   9 | pub fn either(a: i64, b: i64) -> bool { a > 0 || b > 0 }
//  10 | pub fn held(x: i64) -> i64 {
//  11 |     let c = x > 0;
//  12 |     if c { 1 } else { 2 }
//  13 | }

module {
  // A branch region counts the condition. The `else` branch has no op other
  // than its yield, and that yield gives the span of the branch.
  // CHECK-LABEL: func.func @pick(
  // CHECK-SAME: counters = 3 : i32
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("b.rr":1:28 to 7:2)}, {counter = 1 : i32, span = loc("b.rr":3:9 to :14)}, {counter = 2 : i32, span = loc("b.rr":5:9 to :10)}, {false_counter = 2 : i32, span = loc("b.rr":2:8 to :13), true_counter = 1 : i32}]}
  func.func @pick(%x: i64) -> i64 {
    %zero = arith.constant 0 : i64 loc("b.rr":2:12 to 2:13)
    %cond = arith.cmpi sgt, %x, %zero : i64 loc("b.rr":2:8 to 2:13)
    %r = scf.if %cond -> (i64) {
      %one = arith.constant 1 : i64 loc("b.rr":3:13 to 3:14)
      %sum = arith.addi %x, %one : i64 loc("b.rr":3:9 to 3:14)
      scf.yield %sum : i64 loc("b.rr":3:9 to 3:14)
    } else {
      scf.yield %x : i64 loc("b.rr":5:9 to 5:10)
    } loc("b.rr":2:5 to 6:6)
    return %r : i64 loc("b.rr":1:28 to 7:2)
  } loc("b.rr":1:28 to 7:2)

  // The `false` of `a && b` has the location of the `scf.if`. Its branch has
  // a counter for the branch region, but no code region.
  // CHECK-LABEL: func.func @both(
  // CHECK-SAME: counters = 3 : i32
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("b.rr":8:37 to :55)}, {counter = 1 : i32, span = loc("b.rr":8:48 to :53)}, {false_counter = 2 : i32, span = loc("b.rr":8:39 to :44), true_counter = 1 : i32}]}
  // CHECK: } else {
  // CHECK: llvm.mlir.constant(2 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.constant false
  func.func @both(%a: i64, %b: i64) -> i1 {
    %zero = arith.constant 0 : i64 loc("b.rr":8:43 to 8:44)
    %lhs = arith.cmpi sgt, %a, %zero : i64 loc("b.rr":8:39 to 8:44)
    %r = scf.if %lhs -> (i1) {
      %zero2 = arith.constant 0 : i64 loc("b.rr":8:52 to 8:53)
      %rhs = arith.cmpi sgt, %b, %zero2 : i64 loc("b.rr":8:48 to 8:53)
      scf.yield %rhs : i1 loc("b.rr":8:48 to 8:53)
    } else {
      %false = arith.constant false loc("b.rr":8:39 to 8:53)
      scf.yield %false : i1 loc("b.rr":8:39 to 8:53)
    } loc("b.rr":8:39 to 8:53)
    return %r : i1 loc("b.rr":8:37 to 8:55)
  } loc("b.rr":8:37 to 8:55)

  // CHECK-LABEL: func.func @either(
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("b.rr":9:39 to :57)}, {counter = 2 : i32, span = loc("b.rr":9:50 to :55)}, {false_counter = 2 : i32, span = loc("b.rr":9:41 to :46), true_counter = 1 : i32}]}
  func.func @either(%a: i64, %b: i64) -> i1 {
    %zero = arith.constant 0 : i64 loc("b.rr":9:45 to 9:46)
    %lhs = arith.cmpi sgt, %a, %zero : i64 loc("b.rr":9:41 to 9:46)
    %r = scf.if %lhs -> (i1) {
      %true = arith.constant true loc("b.rr":9:41 to 9:55)
      scf.yield %true : i1 loc("b.rr":9:41 to 9:55)
    } else {
      %zero2 = arith.constant 0 : i64 loc("b.rr":9:54 to 9:55)
      %rhs = arith.cmpi sgt, %b, %zero2 : i64 loc("b.rr":9:50 to 9:55)
      scf.yield %rhs : i1 loc("b.rr":9:50 to 9:55)
    } loc("b.rr":9:41 to 9:55)
    return %r : i1 loc("b.rr":9:39 to 9:57)
  } loc("b.rr":9:39 to 9:57)

  // The op that makes the condition is not in the span of the `scf.if`, so
  // the condition has no span and there is no branch region.
  // CHECK-LABEL: func.func @held(
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("b.rr":10:28 to 13:2)}, {counter = 1 : i32, span = loc("b.rr":12:12 to :13)}, {counter = 2 : i32, span = loc("b.rr":12:23 to :24)}]}
  func.func @held(%x: i64) -> i64 {
    %zero = arith.constant 0 : i64 loc("b.rr":11:17 to 11:18)
    %c = arith.cmpi sgt, %x, %zero : i64 loc("b.rr":11:13 to 11:18)
    %r = scf.if %c -> (i64) {
      %one = arith.constant 1 : i64 loc("b.rr":12:12 to 12:13)
      scf.yield %one : i64 loc("b.rr":12:12 to 12:13)
    } else {
      %two = arith.constant 2 : i64 loc("b.rr":12:23 to 12:24)
      scf.yield %two : i64 loc("b.rr":12:23 to 12:24)
    } loc("b.rr":12:5 to 12:26)
    return %r : i64 loc("b.rr":10:28 to 13:2)
  } loc("b.rr":10:28 to 13:2)
}

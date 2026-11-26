// Each region of a `match` dispatch gets a counter. The frontend gives the
// ops of an arm the location of the arm, so the span of the region is the
// span of its arm.

// RUN: %reussir-opt %s --reussir-instrument-coverage --mlir-print-local-scope | %FileCheck %s

!variant = !reussir.record<variant "Shape" {i32, i64}>
!ref = !reussir.ref<!variant field>
!nullable = !reussir.nullable<!reussir.rc<i64>>

module {
  // CHECK-LABEL: func.func @size(
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("m.rr":1:1 to 6:2)}, {counter = 1 : i32, span = loc("m.rr":3:19 to :20)}, {counter = 2 : i32, span = loc("m.rr":4:23 to :28)}]}
  // CHECK: reussir.record.dispatch
  // CHECK: [0] -> {
  // CHECK: llvm.mlir.constant(1 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: reussir.ref.load
  // CHECK: [1] -> {
  // CHECK: llvm.mlir.constant(2 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.constant 7
  func.func @size(%ref: !ref) -> i32 {
    %r = reussir.record.dispatch(%ref : !ref) -> i32 {
      [0] -> {
      ^bb0(%v: !reussir.ref<i32 field>):
        %x = reussir.ref.load(%v : !reussir.ref<i32 field>) : i32 loc("m.rr":3:19 to 3:20)
        reussir.scf.yield %x : i32 loc("m.rr":3:19 to 3:20)
      }
      [1] -> {
      ^bb0(%v: !reussir.ref<i64 field>):
        %c = arith.constant 7 : i32 loc("m.rr":4:23 to 4:28)
        reussir.scf.yield %c : i32 loc("m.rr":4:23 to 4:28)
      }
    } loc("m.rr":2:3 to 5:4)
    return %r : i32 loc("m.rr":2:3 to 5:4)
  } loc("m.rr":1:1 to 6:2)

  // CHECK-LABEL: func.func @read(
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("m.rr":7:1 to 12:2)}, {counter = 1 : i32, span = loc("m.rr":9:33 to :34)}, {counter = 2 : i32, span = loc("m.rr":10:27 to :29)}]}
  // CHECK: reussir.nullable.dispatch
  // CHECK: nonnull -> {
  // CHECK: llvm.mlir.constant(1 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.constant 1 : i64
  // CHECK: null -> {
  // CHECK: llvm.mlir.constant(2 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.constant -1 : i64
  func.func @read(%n: !nullable) -> i64 {
    %r = reussir.nullable.dispatch(%n : !nullable) -> i64 {
      nonnull -> {
      ^bb0(%p: !reussir.rc<i64>):
        %one = arith.constant 1 : i64 loc("m.rr":9:33 to 9:34)
        reussir.scf.yield %one : i64 loc("m.rr":9:33 to 9:34)
      }
      null -> {
        %none = arith.constant -1 : i64 loc("m.rr":10:27 to 10:29)
        reussir.scf.yield %none : i64 loc("m.rr":10:27 to 10:29)
      }
    } loc("m.rr":8:3 to 11:4)
    return %r : i64 loc("m.rr":8:3 to 11:4)
  } loc("m.rr":7:1 to 12:2)

  // The default region of `scf.index_switch` is its first region.
  // CHECK-LABEL: func.func @digit(
  // CHECK-SAME: regions = [{counter = 0 : i32, span = loc("m.rr":13:1 to 18:2)}, {counter = 1 : i32, span = loc("m.rr":16:10 to :12)}, {counter = 2 : i32, span = loc("m.rr":15:10 to :12)}]}
  // CHECK: scf.index_switch
  // CHECK: case 0 {
  // CHECK: llvm.mlir.constant(2 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.constant 10
  // CHECK: default {
  // CHECK: llvm.mlir.constant(1 : i32)
  // CHECK-NEXT: llvm.call_intrinsic "llvm.instrprof.increment"
  // CHECK-NEXT: arith.constant 30
  func.func @digit(%i: index) -> i64 {
    %r = scf.index_switch %i -> i64
    case 0 {
      %ten = arith.constant 10 : i64 loc("m.rr":15:10 to 15:12)
      scf.yield %ten : i64 loc("m.rr":15:10 to 15:12)
    }
    default {
      %thirty = arith.constant 30 : i64 loc("m.rr":16:10 to 16:12)
      scf.yield %thirty : i64 loc("m.rr":16:10 to 16:12)
    } loc("m.rr":14:3 to 17:4)
    return %r : i64 loc("m.rr":14:3 to 17:4)
  } loc("m.rr":13:1 to 18:2)
}

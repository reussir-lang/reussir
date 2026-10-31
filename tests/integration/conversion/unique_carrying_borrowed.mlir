// RUN: %reussir-opt %s --reussir-unique-carrying-recursion-analysis | %FileCheck %s

// A borrowed parameter is never a source of uniqueness: its caller still
// holds the object. The two functions are identical except for the
// attribute; only the owned one carries uniqueness and gets a `.unique`
// clone for its self call.
module {
  func.func @driver(%rc: !reussir.rc<i64>, %n: i64) -> (!reussir.rc<i64>, !reussir.rc<i64>) {
    %a = func.call @owned(%rc, %n) : (!reussir.rc<i64>, i64) -> !reussir.rc<i64>
    %b = func.call @borrowed(%rc, %n) : (!reussir.rc<i64>, i64) -> !reussir.rc<i64>
    return %a, %b : !reussir.rc<i64>, !reussir.rc<i64>
  }

  // CHECK-LABEL: func.func private @owned(
  // CHECK-SAME: attributes {reussir.carrying_uniqueness}
  // CHECK: func.call @owned.unique(
  func.func private @owned(%rc: !reussir.rc<i64>, %n: i64) -> !reussir.rc<i64> {
    %c0 = arith.constant 0 : i64
    %c1 = arith.constant 1 : i64
    %cond = arith.cmpi eq, %n, %c0 : i64
    %result = scf.if %cond -> (!reussir.rc<i64>) {
      scf.yield %rc : !reussir.rc<i64>
    } else {
      %next = reussir.rc.create value(%n : i64) : !reussir.rc<i64>
      %n1 = arith.subi %n, %c1 : i64
      %rec = func.call @owned(%next, %n1) : (!reussir.rc<i64>, i64) -> !reussir.rc<i64>
      scf.yield %rec : !reussir.rc<i64>
    }
    return %result : !reussir.rc<i64>
  }

  // CHECK-LABEL: func.func private @borrowed(
  // CHECK-NOT: reussir.carrying_uniqueness
  // CHECK: func.call @borrowed(
  // CHECK-NOT: @borrowed.unique
  func.func private @borrowed(%rc: !reussir.rc<i64> {reussir.borrowed = #reussir.borrowed}, %n: i64) -> !reussir.rc<i64> {
    %c0 = arith.constant 0 : i64
    %c1 = arith.constant 1 : i64
    %cond = arith.cmpi eq, %n, %c0 : i64
    %result = scf.if %cond -> (!reussir.rc<i64>) {
      scf.yield %rc : !reussir.rc<i64>
    } else {
      %next = reussir.rc.create value(%n : i64) : !reussir.rc<i64>
      %n1 = arith.subi %n, %c1 : i64
      %rec = func.call @borrowed(%next, %n1) : (!reussir.rc<i64>, i64) -> !reussir.rc<i64>
      scf.yield %rec : !reussir.rc<i64>
    }
    return %result : !reussir.rc<i64>
  }
}

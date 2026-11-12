// RUN: %reussir-opt %s --reussir-borrow-specialization --reussir-unique-carrying-recursion-analysis | %FileCheck %s --implicit-check-not='.borrow' --implicit-check-not='.unique'

!R = !reussir.rc<i64>
module {
  // CHECK-LABEL: @retain(
  // CHECK: reussir.rc.inc{{.*}}{reussir.unsafe}
  func.func private @retain(%x: !R) {
    reussir.rc.inc(%x : !R) {reussir.unsafe}
    reussir.rc.dec(%x : !R)
    return
  }
  // CHECK-LABEL: @release(
  // CHECK: reussir.rc.dec{{.*}}{reussir.unsafe}
  func.func private @release(%x: !R) {
    reussir.rc.dec(%x : !R) {reussir.unsafe}
    reussir.rc.dec(%x : !R)
    return
  }
  // CHECK-LABEL: @caller(
  // CHECK: reussir.rc.inc
  // CHECK: call @retain(
  // CHECK: reussir.rc.inc
  // CHECK: call @release(
  func.func @caller(%x: !R) -> i1 {
    reussir.rc.inc(%x : !R)
    func.call @retain(%x) : (!R) -> ()
    reussir.rc.inc(%x : !R)
    func.call @release(%x) : (!R) -> ()
    %u = reussir.rc.is_unique(%x : !R) : i1
    reussir.rc.dec(%x : !R)
    return %u : i1
  }
  // CHECK-LABEL: func.func @release_then_return(
  // CHECK-NOT: reussir.carrying_uniqueness
  // CHECK: reussir.rc.dec{{.*}}{reussir.unsafe}
  // CHECK: return
  func.func @release_then_return(%x: !R) -> !R {
    reussir.rc.dec(%x : !R) {reussir.unsafe}
    return %x : !R
  }
  // CHECK-LABEL: @loop(
  // CHECK: reussir.rc.inc{{.*}}{reussir.unsafe}
  // CHECK: call @loop(
  func.func @loop(%x: !R, %n: i64) -> !R {
    reussir.rc.inc(%x : !R) {reussir.unsafe}
    reussir.rc.dec(%x : !R) {reussir.unsafe}
    %zero = arith.constant 0 : i64
    %one = arith.constant 1 : i64
    %done = arith.cmpi eq, %n, %zero : i64
    %result = scf.if %done -> (!R) {
      scf.yield %x : !R
    } else {
      reussir.rc.dec(%x : !R)
      %next = reussir.rc.create value(%n : i64) : !R
      %n1 = arith.subi %n, %one : i64
      %r = func.call @loop(%next, %n1) : (!R, i64) -> !R
      scf.yield %r : !R
    }
    return %result : !R
  }
}

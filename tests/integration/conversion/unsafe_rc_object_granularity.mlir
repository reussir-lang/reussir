// RUN: %reussir-opt %s --reussir-borrow-specialization | %FileCheck %s --check-prefix=BORROW
// RUN: %reussir-opt %s --reussir-unique-carrying-recursion-analysis | %FileCheck %s --check-prefix=UNIQUE

!R = !reussir.rc<i64>
module {
  func.func private @touch(%manual: !R, %automatic: !R) {
    reussir.rc.inc(%manual : !R) {reussir.unsafe}
    reussir.rc.dec(%manual : !R)
    reussir.rc.dec(%automatic : !R)
    return
  }
  // BORROW-LABEL: @caller(
  // BORROW: reussir.rc.inc(%arg0
  // BORROW-NOT: reussir.rc.inc(%arg1
  // BORROW: call @touch.borrow_1(
  func.func @caller(%manual: !R, %automatic: !R) -> (i1, i1) {
    reussir.rc.inc(%manual : !R)
    reussir.rc.inc(%automatic : !R)
    func.call @touch(%manual, %automatic) : (!R, !R) -> ()
    %m = reussir.rc.is_unique(%manual : !R) : i1
    %a = reussir.rc.is_unique(%automatic : !R) : i1
    reussir.rc.dec(%manual : !R)
    reussir.rc.dec(%automatic : !R)
    return %m, %a : i1, i1
  }
  // UNIQUE-LABEL: @mixed_loop(
  // UNIQUE: call @mixed_loop.unique(
  func.func @mixed_loop(%manual: !R, %automatic: !R, %n: i64) -> (!R, !R) {
    reussir.rc.inc(%manual : !R) {reussir.unsafe}
    reussir.rc.dec(%manual : !R) {reussir.unsafe}
    %zero = arith.constant 0 : i64
    %one = arith.constant 1 : i64
    %done = arith.cmpi eq, %n, %zero : i64
    %m, %a = scf.if %done -> (!R, !R) {
      scf.yield %manual, %automatic : !R, !R
    } else {
      reussir.rc.dec(%automatic : !R)
      %next = reussir.rc.create value(%n : i64) : !R
      %n1 = arith.subi %n, %one : i64
      %r0, %r1 = func.call @mixed_loop(%manual, %next, %n1) : (!R, !R, i64) -> (!R, !R)
      scf.yield %r0, %r1 : !R, !R
    }
    return %m, %a : !R, !R
  }
  // BORROW-LABEL: @touch.borrow_1(
  // BORROW: reussir.rc.inc(%arg0{{.*}}{reussir.unsafe}
  // BORROW: reussir.rc.dec(%arg0
  // BORROW-NOT: reussir.rc.dec(%arg1
  // BORROW: return
  // UNIQUE-LABEL: @mixed_loop.unique(
  // UNIQUE-NOT: reussir.rc.assume_unique(%arg0
  // UNIQUE: reussir.rc.assume_unique(%arg1
  // UNIQUE: reussir.rc.inc(%arg0{{.*}}{reussir.unsafe}
  // UNIQUE: reussir.rc.dec(%arg0{{.*}}{reussir.unsafe}
}

// RUN: %reussir-opt %s --reussir-borrow-specialization | %FileCheck %s --check-prefix=BORROW

!R = !reussir.rc<i64>
module {
  func.func private @touch(%manual: !R, %automatic: !R) {
    reussir.rc.inc(%manual : !R) {unsafe}
    reussir.rc.dec(%manual : !R)
    reussir.rc.dec(%automatic : !R)
    return
  }
  // BORROW-LABEL: @caller(
  // BORROW: reussir.rc.inc(%arg0
  // BORROW-NOT: reussir.rc.inc(%arg1
  // BORROW: call @touch.borrowed_1(
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
  // BORROW-LABEL: func.func private @touch.borrowed_1(
  // BORROW: reussir.rc.inc(%arg0{{.*}}{unsafe}
  // BORROW: reussir.rc.dec(%arg0
  // BORROW-NOT: reussir.rc.dec(%arg1
  // BORROW: return
}

// RUN: %reussir-opt %s --reussir-inc-dec-cancellation | %FileCheck %s
// RUN: %reussir-opt %s --reussir-inc-dec-cancellation | %reussir-opt --reussir-inc-dec-cancellation | %FileCheck %s

!R = !reussir.rc<i64>
module {
  // CHECK-LABEL: @manual_pair(
  // CHECK: reussir.rc.inc{{.*}}{unsafe}
  // CHECK: reussir.rc.borrow
  // CHECK: reussir.rc.dec{{.*}}{unsafe}
  func.func @manual_pair(%x: !R) -> i64 {
    reussir.rc.inc(%x : !R) {unsafe}
    %p = reussir.rc.borrow(%x : !R) : !reussir.ref<i64 shared>
    %v = reussir.ref.load(%p : !reussir.ref<i64 shared>) : i64
    reussir.rc.dec(%x : !R) {unsafe}
    return %v : i64
  }
  // CHECK-LABEL: @manual_inc(
  // CHECK: reussir.rc.inc{{.*}}{unsafe}
  // CHECK: reussir.rc.dec
  func.func @manual_inc(%x: !R) {
    reussir.rc.inc(%x : !R) {unsafe}
    reussir.rc.dec(%x : !R)
    return
  }
  // CHECK-LABEL: @manual_dec(
  // CHECK: reussir.rc.inc
  // CHECK: reussir.rc.dec{{.*}}{unsafe}
  func.func @manual_dec(%x: !R) {
    reussir.rc.inc(%x : !R)
    reussir.rc.dec(%x : !R) {unsafe}
    return
  }
  // CHECK-LABEL: @barrier(
  // CHECK: reussir.rc.inc
  // CHECK: reussir.rc.inc{{.*}}{unsafe}
  // CHECK: reussir.rc.dec
  func.func @barrier(%x: !R) {
    reussir.rc.inc(%x : !R)
    reussir.rc.inc(%x : !R) {unsafe}
    reussir.rc.dec(%x : !R)
    return
  }
  // CHECK-LABEL: @branches(
  // CHECK: reussir.rc.inc
  // CHECK: scf.if
  // CHECK: reussir.rc.dec{{.*}}{unsafe}
  // CHECK: reussir.rc.dec{{.*}}{unsafe}
  func.func @branches(%x: !R, %c: i1) {
    reussir.rc.inc(%x : !R)
    scf.if %c {
      reussir.rc.dec(%x : !R) {unsafe}
    } else {
      reussir.rc.dec(%x : !R) {unsafe}
    }
    return
  }
  // CHECK-LABEL: @ordinary(
  // CHECK-NOT: reussir.rc.
  // CHECK: return
  func.func @ordinary(%x: !R) {
    reussir.rc.inc(%x : !R)
    reussir.rc.dec(%x : !R)
    return
  }
}

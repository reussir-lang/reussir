// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-rc-decrement-expansion --canonicalize --cse --reussir-inc-dec-cancellation | %FileCheck %s

!R = !reussir.rc<i64>
!A = !reussir.rc<i64 shared atomic>
module attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>>} {
  // CHECK-LABEL: @manual_pair(
  // CHECK: reussir.rc.inc{{.*}}{unsafe}
  // CHECK: reussir.rc.fetch{{.*}}{unsafe}
  // CHECK: reussir.rc.set
  func.func @manual_pair(%x: !R) {
    reussir.rc.inc(%x : !R) {unsafe}
    reussir.rc.dec(%x : !R) {unsafe}
    return
  }
  // CHECK-LABEL: @atomic_pair(
  // CHECK: reussir.rc.inc{{.*}}{unsafe}
  // CHECK: reussir.rc.fetch_sub{{.*}}{unsafe}
  func.func @atomic_pair(%x: !A) {
    reussir.rc.inc(%x : !A) {unsafe}
    reussir.rc.dec(%x : !A) {unsafe}
    return
  }
}

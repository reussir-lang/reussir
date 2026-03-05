// RUN: %reussir-opt %s | %FileCheck %s --check-prefix=CHECK-BEFORE
// RUN: %reussir-opt %s --reussir-inc-dec-cancellation | %FileCheck %s --check-prefix=CHECK-AFTER

!rc64 = !reussir.rc<i64>
!nullable_token_rc64 = !reussir.nullable<!reussir.token<align: 8, size: 16>>

module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>> } {
  func.func @nested_if_cancel(%arg0: !rc64, %cond0: i1, %cond1: i1) {
    reussir.rc.inc (%arg0 : !rc64)
    "scf.if"(%cond0) ({
      scf.if %cond1 {
        %tok0 = reussir.rc.dec (%arg0 : !rc64) : !nullable_token_rc64
        reussir.token.free (%tok0 : !nullable_token_rc64)
        scf.yield
      } else {
        %tok1 = reussir.rc.dec (%arg0 : !rc64) : !nullable_token_rc64
        reussir.token.free (%tok1 : !nullable_token_rc64)
        scf.yield
      }
      scf.yield
    }, {
      scf.yield
    }) {reussir.expanded_decrement} : (i1) -> ()
    return
  }
}

// CHECK-BEFORE-LABEL: func.func @nested_if_cancel
// CHECK-BEFORE: reussir.rc.inc
// CHECK-BEFORE: reussir.rc.dec
// CHECK-BEFORE: reussir.rc.dec

// CHECK-AFTER-LABEL: func.func @nested_if_cancel
// CHECK-AFTER: scf.if %{{.*}} {
// CHECK-AFTER: } else {
// CHECK-AFTER: reussir.rc.inc
// CHECK-AFTER-NOT: reussir.rc.dec

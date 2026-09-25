// RUN: %reussir-opt %s --reussir-inc-dec-cancellation | %FileCheck %s

// CHECK-LABEL: func.func @multiple
// CHECK: reussir.rc.inc{{.*}} by %arg1
// CHECK: reussir.rc.dec
func.func @multiple(%rc: !reussir.rc<i32>, %n: index) {
  reussir.rc.inc(%rc : !reussir.rc<i32>) by %n
  reussir.rc.dec(%rc : !reussir.rc<i32>)
  return
}
// CHECK-LABEL: func.func @one
// CHECK-NOT: reussir.rc.inc
// CHECK-NOT: reussir.rc.dec
// CHECK: return
func.func @one(%rc: !reussir.rc<i32>) {
  %one = arith.constant 1 : index
  reussir.rc.inc(%rc : !reussir.rc<i32>) by %one
  reussir.rc.dec(%rc : !reussir.rc<i32>)
  return
}

// CHECK-LABEL: func.func @conditional_one
// CHECK-NOT: reussir.rc.inc
// CHECK: scf.if %arg1
// CHECK-NEXT: } else {
// CHECK-NEXT: reussir.rc.inc
// CHECK-NOT: reussir.rc.dec
// CHECK: return
func.func @conditional_one(%rc: !reussir.rc<i32>, %condition: i1) {
  reussir.rc.inc(%rc : !reussir.rc<i32>)
  scf.if %condition {
    reussir.rc.dec(%rc : !reussir.rc<i32>)
  } {reussir.expanded_decrement}
  return
}

// RUN: %reussir-opt %s --reussir-inc-dec-cancellation | %FileCheck %s

!R = !reussir.rc<i64>

// Retain only the value that escapes from a borrowed lookup.
// CHECK-LABEL: func.func @else_releases(
// CHECK-NOT: reussir.rc.inc
// CHECK: scf.if
// CHECK-NEXT: reussir.rc.inc(%arg0
// CHECK-NEXT: scf.yield %arg0
// CHECK: } else {
// CHECK-NOT: reussir.rc.dec
// CHECK: scf.yield %arg1
func.func @else_releases(%value: !R, %other: !R, %found: i1) -> !R {
  reussir.rc.inc(%value : !R)
  %r = scf.if %found -> (!R) {
    scf.yield %value : !R
  } else {
    reussir.rc.dec(%value : !R)
    scf.yield %other : !R
  }
  return %r : !R
}

// CHECK-LABEL: func.func @then_releases(
// CHECK-NOT: reussir.rc.inc
// CHECK: scf.if
// CHECK-NOT: reussir.rc.dec
// CHECK: scf.yield %arg1
// CHECK: } else {
// CHECK-NEXT: reussir.rc.inc(%arg0
// CHECK-NEXT: scf.yield %arg0
func.func @then_releases(%value: !R, %other: !R, %found: i1) -> !R {
  reussir.rc.inc(%value : !R)
  %r = scf.if %found -> (!R) {
    reussir.rc.dec(%value : !R)
    scf.yield %other : !R
  } else {
    scf.yield %value : !R
  }
  return %r : !R
}

// CHECK-LABEL: func.func @both_release(
// CHECK-NOT: reussir.rc.inc
// CHECK-NOT: reussir.rc.dec
// CHECK: return
func.func @both_release(%value: !R, %found: i1) {
  reussir.rc.inc(%value : !R)
  scf.if %found {
    reussir.rc.dec(%value : !R)
  } else {
    reussir.rc.dec(%value : !R)
  }
  return
}

// The count observation before a release must still see the retain.
// CHECK-LABEL: func.func @observes_count(
// CHECK: reussir.rc.inc(%arg0
// CHECK: scf.if
// CHECK: } else {
// CHECK-NEXT: {{.*}}reussir.rc.is_unique
// CHECK-NEXT: reussir.rc.dec(%arg0
func.func @observes_count(%value: !R, %other: !R, %found: i1) -> !R {
  reussir.rc.inc(%value : !R)
  %r = scf.if %found -> (!R) {
    scf.yield %value : !R
  } else {
    %unique = reussir.rc.is_unique(%value : !R) : i1
    reussir.rc.dec(%value : !R)
    scf.yield %other : !R
  }
  return %r : !R
}

// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-rc-decrement-expansion{defer-normal=true},reussir-acquire-drop-expansion,func.func(reussir-inc-dec-cancellation,reussir-token-reuse),reussir-acquire-drop-expansion{outline-record=true expand-decrement=true})' --verify-each | %FileCheck %s --implicit-check-not=reussir.drop.run

!leaf = !reussir.record<compound "Leaf" { i64 }>
!child = !reussir.rc<!leaf>
!owner = !reussir.record<compound "Owner" { !leaf }>
!rc = !reussir.rc<!owner>
!token = !reussir.token<align: 8, size: 16>
!nullable = !reussir.nullable<!token>

// CHECK-LABEL: func.func @reuse
// CHECK-NOT: reussir.rc.inc
// CHECK: scf.if
// CHECK-NOT: reussir.rc.inc
// CHECK-NOT: reussir.rc.dec
// CHECK: reussir.rc.reinterpret
// CHECK: } else {
// CHECK: reussir.rc.inc
// CHECK: reussir.token.ensure
func.func @reuse(%owner: !rc) -> !rc {
  %ref = reussir.rc.borrow(%owner : !rc) : !reussir.ref<!owner>
  %slot = reussir.ref.project(%ref : !reussir.ref<!owner>)[0] : !reussir.ref<!child>
  %child = reussir.ref.load(%slot : !reussir.ref<!child>) : !child
  reussir.rc.inc(%child : !child)
  %donor = reussir.rc.dec(%owner : !rc) : !nullable
  %value = reussir.record.compound(%child : !child) : !owner
  %token = reussir.token.alloc : !token
  %result = reussir.rc.create value(%value : !owner) token(%token : !token) : !rc
  return %result : !rc
}

// CHECK-LABEL: func.func @no_reuse
// CHECK-NOT: reussir.rc.inc
// CHECK: scf.if
// CHECK-NOT: reussir.rc.inc
// CHECK-NOT: reussir.rc.dec
// CHECK: reussir.rc.reinterpret
// CHECK: } else {
// CHECK: reussir.rc.inc
func.func @no_reuse(%owner: !rc) -> !child {
  %ref = reussir.rc.borrow(%owner : !rc) : !reussir.ref<!owner>
  %slot = reussir.ref.project(%ref : !reussir.ref<!owner>)[0] : !reussir.ref<!child>
  %child = reussir.ref.load(%slot : !reussir.ref<!child>) : !child
  reussir.rc.inc(%child : !child)
  %donor = reussir.rc.dec(%owner : !rc) : !nullable
  reussir.token.free(%donor : !nullable)
  return %child : !child
}

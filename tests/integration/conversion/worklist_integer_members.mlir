// REQUIRES: rcgmp
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true' --verify-each | %FileCheck %s
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --verify-each | %FileCheck %s --check-prefix=FORWARD

// An integer member is released in place: an arm of integers is a leaf, and
// under backward order an arm of integers and boxes continues in its last
// dead box. An arm with a foreign object keeps the ordered release.

!handle = !reussir.rc<!reussir.ffi_object<"Handle", @handle_drop>>
!expr_ = !reussir.record<variant "Expr" incomplete>
!val = !reussir.record<compound "Expr::Val" [value] { !rcgmp.mpz }>
!var = !reussir.record<compound "Expr::Var" [value] { !handle }>
!add = !reussir.record<compound "Expr::Add" [value] { !expr_, !rcgmp.mpz, !expr_ }>
!expr = !reussir.record<variant "Expr" { !val, !var, !add }>
!token = !reussir.nullable<!reussir.token<align: 8, size: ?>>

module {
  func.func private @handle_drop(!handle)

  // CHECK-LABEL: REUSSIR_DROP_release_backward{{.*}}: !reussir.rc<!reussir.record<variant "Expr"
  // CHECK: ^bb0(%{{.*}}"Expr::Val"
  // CHECK: rcgmp.release
  // CHECK-NEXT: reussir.rc.reinterpret
  // CHECK-NEXT: reussir.token.free
  // CHECK: ^bb0(%{{.*}}"Expr::Var"
  // CHECK: reussir.drop.defer
  // CHECK: reussir.token.free
  // CHECK: ^bb0(%{{.*}}"Expr::Add"
  // CHECK: rcgmp.release
  // CHECK: reussir.token.free
  // CHECK: reussir.rc.defer
  // CHECK: {reussir.musttail}
  // CHECK: {reussir.musttail}
  // CHECK: return
  // CHECK-LABEL: REUSSIR_DROP_entry_backward{{.*}}: !reussir.ref<!reussir.record<variant "Expr"
  // CHECK: ^bb0(%{{.*}}"Expr::Val"
  // CHECK: rcgmp.release
  // CHECK-NEXT: reussir.scf.yield
  // CHECK: reussir.drop.run
  // CHECK: reussir.drop.run

  // FORWARD-LABEL: REUSSIR_DROP_release_forward{{.*}}: !reussir.rc<!reussir.record<variant "Expr"
  // FORWARD-NOT: rcgmp.release
  // FORWARD: call @{{.*}}REUSSIR_DROP_walk_forward
  // FORWARD-LABEL: REUSSIR_DROP_entry_forward{{.*}}: !reussir.ref<!reussir.record<variant "Expr"
  // FORWARD: ^bb0(%{{.*}}"Expr::Val"
  // FORWARD: rcgmp.release
  // FORWARD-NEXT: reussir.scf.yield
  func.func @expr(%rc: !reussir.rc<!expr>) {
    %token = reussir.rc.dec(%rc : !reussir.rc<!expr>) : !token
    reussir.token.free(%token : !token)
    return
  }
}

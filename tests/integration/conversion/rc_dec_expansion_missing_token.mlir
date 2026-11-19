// RUN: %reussir-opt %s --reussir-rc-decrement-expansion \
// RUN:   | %FileCheck %s --implicit-check-not=reussir.rc.dec
// RUN: %reussir-opt %s \
// RUN:   --reussir-acquire-drop-expansion='expand-decrement=1 outline-record=1' \
// RUN:   | %FileCheck %s --check-prefix=OUTLINE \
// RUN:     --implicit-check-not=reussir.rc.dec
//
// A decrement without a token result is the form before
// `reussir-token-instantiation`. The expansion gives it the token that the
// producer phase would: the box size, the dynamic size for an unpinned variant
// with arms of 16 and 24 bytes, and the arm's cell for a pinned one.

!leaf = !reussir.record<compound "Leaf" { i64, f64 }>
!v_a = !reussir.record<compound "V::A" [value] { i64 }>
!v_b = !reussir.record<compound "V::B" [value] { i64, i64 }>
!v = !reussir.record<variant "V" { !v_a, !v_b }>

module {
  // CHECK-LABEL: func.func @leaf(
  // CHECK: %[[COUNT:.*]] = reussir.rc.fetch(%[[RC:.*]] : !reussir.rc
  // CHECK: scf.if %{{.*}} -> (!reussir.nullable<!reussir.token<align : 8, size : 24>>) {
  // CHECK: reussir.ref.drop
  // CHECK: %[[TOKEN:.*]] = reussir.rc.reinterpret(%[[RC]] : {{.*}}) : !reussir.token<align : 8, size : 24>
  // CHECK: %[[SOME:.*]] = reussir.nullable.create(%[[TOKEN]] :
  // CHECK: scf.yield %[[SOME]]
  // CHECK: } else {
  // CHECK: reussir.rc.set(%[[RC]] :
  // CHECK: %[[NONE:.*]] = reussir.nullable.create : <!reussir.token<align : 8, size : 24>>
  // CHECK: scf.yield %[[NONE]]
  // CHECK: } {reussir.expanded_decrement}
  // OUTLINE-LABEL: func.func @leaf(
  // OUTLINE: scf.if %{{.*}} -> (!reussir.nullable<!reussir.token<align : 8, size : 24>>) {
  // OUTLINE: } {reussir.expanded_decrement}
  func.func @leaf(%rc: !reussir.rc<!leaf>) {
    reussir.rc.dec(%rc : !reussir.rc<!leaf>)
    return
  }

  // CHECK-LABEL: func.func @unpinned(
  // CHECK: scf.if %{{.*}} -> (!reussir.nullable<!reussir.token<align : 8, size : ?>>) {
  // CHECK: reussir.ref.drop
  // CHECK: } {reussir.expanded_decrement}
  func.func @unpinned(%rc: !reussir.rc<!v>) {
    reussir.rc.dec(%rc : !reussir.rc<!v>)
    return
  }

  // CHECK-LABEL: func.func @pinned(
  // CHECK: scf.if %{{.*}} -> (!reussir.nullable<!reussir.token<align : 8, size : 16>>) {
  // CHECK-NOT: reussir.ref.drop
  // CHECK: } {reussir.expanded_decrement}
  func.func @pinned(%rc: !reussir.rc<!v>) {
    reussir.rc.dec(%rc : !reussir.rc<!v>) {boundMembers = array<i64: 0>, destructureTag = 0 : index}
    return
  }

  // The body of a foreign release function, as `rrc --emit mlir` prints it.
  // CHECK-LABEL: func.func @release(
  // CHECK: scf.if %{{.*}} -> (!reussir.nullable<!reussir.token<align : 8, size : 24>>) {
  // CHECK: } {reussir.expanded_decrement}
  // OUTLINE-LABEL: func.func @release(
  // OUTLINE: reussir.rc.fetch
  func.func @release(%rc: !reussir.rc<!leaf>) attributes {reussir.foreign_drop} {
    reussir.rc.dec(%rc : !reussir.rc<!leaf>)
    return
  }
}

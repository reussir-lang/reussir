// RUN: %reussir-opt %s --reussir-rc-decrement-expansion | %FileCheck %s
// RUN: %reussir-opt %s \
// RUN:   --reussir-acquire-drop-expansion='expand-decrement=1 outline-record=1' \
// RUN:   | %FileCheck %s
// RUN: %reussir-opt %s --reussir-token-instantiation \
// RUN:   --reussir-rc-decrement-expansion \
// RUN:   | %FileCheck %s --check-prefix=TOKEN \
// RUN:     --implicit-check-not=reussir.rc.dec
//
// A decrement gets its token result from `reussir-token-instantiation`, which
// runs before the expansion. Without that pass the expansion leaves the
// decrement as it is; with it, the decrement is expanded.

!leaf = !reussir.record<compound "Leaf" { i64, f64 }>
!v_a = !reussir.record<compound "V::A" [value] { i64 }>
!v_b = !reussir.record<compound "V::B" [value] { i64, i64 }>
!v = !reussir.record<variant "V" { !v_a, !v_b }>

module {
  // CHECK-LABEL: func.func @leaf(
  // CHECK-NEXT: reussir.rc.dec(%{{.*}} : !reussir.rc<!reussir.record<compound "Leaf" {i64, f64}>>){{ *$}}
  // CHECK-NEXT: return
  // TOKEN-LABEL: func.func @leaf(
  // TOKEN: reussir.rc.fetch
  // TOKEN: scf.if %{{.*}} -> (!reussir.nullable<!reussir.token<align : 8, size : 24>>) {
  // TOKEN: } {reussir.expanded_decrement}
  func.func @leaf(%rc: !reussir.rc<!leaf>) {
    reussir.rc.dec(%rc : !reussir.rc<!leaf>)
    return
  }

  // CHECK-LABEL: func.func @pinned(
  // CHECK-NEXT: reussir.rc.dec(%{{.*}}) {boundMembers = array<i64: 0>, destructureTag = 0 : index}
  // CHECK-NEXT: return
  // TOKEN-LABEL: func.func @pinned(
  // TOKEN: scf.if %{{.*}} -> (!reussir.nullable<!reussir.token<align : 8, size : 16>>) {
  // TOKEN: } {reussir.expanded_decrement}
  func.func @pinned(%rc: !reussir.rc<!v>) {
    reussir.rc.dec(%rc : !reussir.rc<!v>) {boundMembers = array<i64: 0>, destructureTag = 0 : index}
    return
  }
}

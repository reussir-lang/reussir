// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-rc-decrement-expansion,reussir-acquire-drop-expansion{expand-decrement=1})' | %FileCheck %s

// Expanding the release of a uniquely owned node reloads its members to drop
// them. A member the function already loaded to retain is read from the same
// immutable storage, so the expansion reuses that load: the retain and the
// release then name one SSA value, which lets later passes pair them.

!leaf = !reussir.record<compound "Leaf" { i64 }>
!node = !reussir.record<compound "Node" { !leaf, i64 }>

module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>> } {
  // CHECK-LABEL: func.func @reuse_loaded_member
  // CHECK: %[[CHILD:.+]] = reussir.ref.load
  // CHECK-NOT: reussir.ref.load
  // CHECK: reussir.rc.inc(%[[CHILD]]
  // The child's release on the unique path, expanded, reads the same load.
  // CHECK: scf.if
  // CHECK-NOT: reussir.ref.load
  // CHECK: reussir.rc.fetch(%[[CHILD]]
  func.func @reuse_loaded_member(%n: !reussir.rc<!node>) -> !reussir.rc<!leaf> {
    %b = reussir.rc.borrow(%n : !reussir.rc<!node>) : !reussir.ref<!node shared>
    %p = reussir.ref.project(%b : !reussir.ref<!node shared>) [0] : !reussir.ref<!reussir.rc<!leaf> shared>
    %c = reussir.ref.load(%p : !reussir.ref<!reussir.rc<!leaf> shared>) : !reussir.rc<!leaf>
    reussir.rc.inc(%c : !reussir.rc<!leaf>)
    %t = reussir.rc.dec(%n : !reussir.rc<!node>) : !reussir.nullable<!reussir.token<align: 8, size: 24>>
    return %c : !reussir.rc<!leaf>
  }
}

// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true retcon-drops=true' --verify-each | %FileCheck %s
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true retcon-drops=false' --verify-each | %FileCheck %s --check-prefix=DIRECT --implicit-check-not=reussir.dtor.suspend

!tree_ = !reussir.record<variant "LeafTree" incomplete>
!empty = !reussir.record<compound "Empty" [value] {}>
!box = !reussir.record<compound "Box" {i64}>
!leaf = !reussir.record<compound "Leaf" [value] {!box}>
!branch = !reussir.record<compound "Branch" [value] {!tree_, !tree_}>
!tree = !reussir.record<variant "LeafTree" {!empty, !leaf, !branch}>

module {
  // CHECK-LABEL: func.func @root(
  // CHECK: reussir.record.dispatch
  // CHECK: [0] ->
  // CHECK-NOT: call
  // CHECK-NOT: reussir.dtor.suspend
  // CHECK: reussir.scf.yield
  // CHECK: [1] ->
  // CHECK-NOT: .retcon
  // CHECK: reussir.rc.fetch
  // CHECK: reussir.rc.reinterpret
  // CHECK: [2] ->
  // CHECK: call @{{.*}}LeafTreeE.retcon
  // CHECK: return
  // DIRECT-LABEL: func.func @root(
  // DIRECT-NOT: reussir.record.dispatch
  // DIRECT: call @{{.*}}LeafTreeE(
  func.func @root(%ref: !reussir.ref<!tree>) {
    reussir.ref.drop (%ref : !reussir.ref<!tree>)
    return
  }

  // CHECK-LABEL: func.func @parent(
  // CHECK: reussir.record.dispatch
  // CHECK: [0] ->
  // CHECK-NOT: call
  // CHECK-NOT: reussir.dtor.suspend
  // CHECK: reussir.scf.yield
  // CHECK: [1] ->
  // CHECK-NOT: .retcon
  // CHECK: reussir.rc.fetch
  // CHECK: reussir.rc.reinterpret
  // CHECK: [2] ->
  // CHECK: reussir.dtor.suspend @{{.*}}LeafTreeE.retcon
  // CHECK: return
  func.func @parent(%ref: !reussir.ref<!tree>) attributes {reussir.retcon_drop} {
    reussir.ref.drop (%ref : !reussir.ref<!tree>)
    return
  }
}

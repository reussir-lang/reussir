// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true' --verify-each | %FileCheck %s --implicit-check-not=' cf.'
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true specialize-recursive-drops=false' --verify-each | %FileCheck %s --check-prefix=UNSPECIALIZED --implicit-check-not=tail_release --implicit-check-not=reussir.musttail
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s --check-prefix=EARLY

!node_ = !reussir.record<compound "Node" incomplete>
!node = !reussir.record<compound "Node" { i64, !reussir.nullable<!reussir.rc<!node_ shared>> }>
!leaf = !reussir.record<compound "Leaf" { i64 }>
!tree_ = !reussir.record<compound "Tree" incomplete>
!tree = !reussir.record<compound "Tree" { !tree_, !tree_ }>
!snoc_ = !reussir.record<compound "Snoc" incomplete>
!snoc = !reussir.record<compound "Snoc" { !snoc_, !leaf }>

// CHECK-LABEL: func.func private @{{.*}}NodeE.tail_release
// CHECK: reussir.rc.fetch
// CHECK: scf.if
// CHECK: %[[NEXT:.*]] = reussir.ref.load
// CHECK: reussir.rc.reinterpret
// CHECK-NEXT: reussir.token.free
// CHECK: scf.if
// CHECK: reussir.nullable.coerce
// CHECK-NEXT: func.call @{{.*}}NodeE.tail_release{{.*}}reussir.musttail
// CHECK: reussir.rc.set
module {
  // UNSPECIALIZED-LABEL: func.func @release
  // UNSPECIALIZED: reussir.rc.dec
  // CHECK-LABEL: func.func @release
  // CHECK: call @{{.*}}NodeE.tail_release
  // EARLY-NOT: tail_release
  func.func @release(%ref: !reussir.ref<!reussir.rc<!node shared>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.rc<!node shared>>)
    return
  }
  // CHECK-LABEL: func.func @other_drops
  // CHECK-NOT: tail_release
  // CHECK: reussir.rc.dec
  // CHECK: call @{{.*}}TreeE.worklist_release
  // CHECK: call @{{.*}}SnocE.worklist_release
  func.func @other_drops(%a: !reussir.ref<!reussir.rc<!leaf shared>>,
                         %b: !reussir.ref<!reussir.rc<!tree shared>>,
                         %c: !reussir.ref<!reussir.rc<!snoc shared>>) {
    reussir.ref.drop (%a : !reussir.ref<!reussir.rc<!leaf shared>>)
    reussir.ref.drop (%b : !reussir.ref<!reussir.rc<!tree shared>>)
    reussir.ref.drop (%c : !reussir.ref<!reussir.rc<!snoc shared>>)
    return
  }
  // CHECK-LABEL: func.func @nullable_release
  // CHECK: call @{{.*}}NodeE.tail_release
  // UNSPECIALIZED-LABEL: func.func @nullable_release
  // UNSPECIALIZED: reussir.rc.dec
  func.func @nullable_release(%ref: !reussir.ref<!reussir.nullable<!reussir.rc<!node shared>>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.nullable<!reussir.rc<!node shared>>>)
    return
  }
}

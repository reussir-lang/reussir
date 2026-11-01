// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true' --verify-each | %FileCheck %s --implicit-check-not=' cf.'
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true tail-recursive-drops=false' --verify-each | %FileCheck %s --check-prefix=UNSPECIALIZED --implicit-check-not=tail_release --implicit-check-not=reussir.musttail
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s --check-prefix=EARLY
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true drop-order=backward' --verify-each | %FileCheck %s --check-prefix=BACKWARD

!node_ = !reussir.record<compound "Node" incomplete>
!node = !reussir.record<compound "Node" { i64, !reussir.nullable<!reussir.rc<!node_ shared>> }>
!leaf = !reussir.record<compound "Leaf" { i64 }>
!color = !reussir.record<variant "Color" [value] { i8, i16 }>
!tree_ = !reussir.record<compound "Tree" incomplete>
!tree = !reussir.record<compound "Tree" { !tree_, !tree_, !color }>
!snoc_ = !reussir.record<compound "Snoc" incomplete>
!snoc = !reussir.record<compound "Snoc" { !snoc_, !leaf }>

// CHECK-LABEL: func.func private @__reussir_drop.release.forward
// CHECK-SAME: "Tree"
// CHECK: reussir.rc.borrow
// CHECK-NEXT: reussir.ref.project{{.*}}[0]
// CHECK-NEXT: [[LEFT:%[0-9]+]] = reussir.ref.load
// CHECK: reussir.ref.project{{.*}}[1]
// CHECK: [[RIGHT:%[0-9]+]] = reussir.ref.load
// CHECK: reussir.token.free
// CHECK: reussir.rc.defer @__reussir_drop.release.forward{{[^ (]*}}([[RIGHT]]
// CHECK: call @__reussir_drop.release.forward{{[^ (]*}}([[LEFT]]) {reussir.musttail}
// CHECK-NOT: reussir.rc.defer
// CHECK: call @__reussir_drop.release.forward{{[^ (]*}}([[RIGHT]]) {reussir.musttail}
// CHECK: return
// BACKWARD-LABEL: func.func private @__reussir_drop.release.backward
// BACKWARD-SAME: "Tree"
// BACKWARD: reussir.rc.borrow
// BACKWARD-NEXT: reussir.ref.project{{.*}}[0]
// BACKWARD-NEXT: [[LEFT:%[0-9]+]] = reussir.ref.load
// BACKWARD: reussir.ref.project{{.*}}[1]
// BACKWARD: [[RIGHT:%[0-9]+]] = reussir.ref.load
// BACKWARD: reussir.token.free
// BACKWARD: reussir.rc.defer @__reussir_drop.release.backward{{[^ (]*}}([[LEFT]]
// BACKWARD: call @__reussir_drop.release.backward{{[^ (]*}}([[RIGHT]]) {reussir.musttail}
// BACKWARD-NOT: reussir.rc.defer
// BACKWARD: call @__reussir_drop.release.backward{{[^ (]*}}([[LEFT]]) {reussir.musttail}
// BACKWARD: return
module {
  // UNSPECIALIZED-LABEL: func.func @release
  // UNSPECIALIZED: reussir.drop.run
  // CHECK-LABEL: func.func @release
  // CHECK-NOT: reussir.drop.run
  // CHECK: call @{{.*}}NodeE.tail_release
  // CHECK-NEXT: return
  // EARLY-NOT: tail_release
  func.func @release(%ref: !reussir.ref<!reussir.rc<!node shared>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.rc<!node shared>>)
    return
  }
  // CHECK-LABEL: func.func @other_drops
  // CHECK: reussir.rc.fetch
  // CHECK: reussir.token.free
  // CHECK: reussir.drop.run
  // CHECK: reussir.drop.run
  func.func @other_drops(%a: !reussir.ref<!reussir.rc<!leaf shared>>,
                         %b: !reussir.ref<!reussir.rc<!tree shared>>,
                         %c: !reussir.ref<!reussir.rc<!snoc shared>>) {
    reussir.ref.drop (%a : !reussir.ref<!reussir.rc<!leaf shared>>)
    reussir.ref.drop (%b : !reussir.ref<!reussir.rc<!tree shared>>)
    reussir.ref.drop (%c : !reussir.ref<!reussir.rc<!snoc shared>>)
    return
  }
  // CHECK-LABEL: func.func @nullable_release
  // CHECK-NOT: reussir.drop.run
  // CHECK: reussir.nullable.dispatch
  // CHECK: call @{{.*}}NodeE.tail_release
  // CHECK-NOT: reussir.drop.run
  // CHECK: return
  // UNSPECIALIZED-LABEL: func.func @nullable_release
  // UNSPECIALIZED: reussir.drop.run
  func.func @nullable_release(%ref: !reussir.ref<!reussir.nullable<!reussir.rc<!node shared>>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.nullable<!reussir.rc<!node shared>>>)
    return
  }
}

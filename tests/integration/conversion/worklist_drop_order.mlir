// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --verify-each | %FileCheck %s --check-prefix=FORWARD
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=backward' --verify-each | %FileCheck %s --check-prefix=BACKWARD
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true tail-recursive-drops=false' --verify-each | %FileCheck %s --check-prefix=FORWARD --implicit-check-not=reussir.musttail

!leaf = !reussir.record<compound "Leaf" { i64 }>
!pair = !reussir.record<compound "Pair" { !leaf, !leaf }>
!array = !reussir.array<2 x 3 x !reussir.rc<!leaf shared atomic>>
!foreign = !reussir.rc<!reussir.ffi_object<"Foreign", @foreign_drop>>

module {
  func.func private @foreign_drop(!foreign)

  // A forward member's decrement runs only after earlier member work finishes.
  // FORWARD-LABEL: func.func @record
  // FORWARD: reussir.ref.project{{.*}}[1]
  // FORWARD-NEXT: reussir.drop.defer
  // FORWARD: reussir.ref.project{{.*}}[0]
  // FORWARD-NEXT: reussir.drop.defer
  // BACKWARD-LABEL: func.func @record
  // BACKWARD: reussir.ref.project{{.*}}[0]
  // BACKWARD: reussir.rc.fetch
  // BACKWARD: reussir.rc.defer
  // BACKWARD: reussir.ref.project{{.*}}[1]
  // BACKWARD: reussir.rc.fetch
  // BACKWARD: reussir.rc.defer
  func.func @record(%ref: !reussir.ref<!pair>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!pair>) inlined
    return
  }

  // The indexed callback queues its successor before decrementing this element.
  // FORWARD-LABEL: func.func @array
  // FORWARD: reussir.drop.defer_indexed @__reussir_drop.array.forward
  // FORWARD-NOT: scf.for
  // BACKWARD-LABEL: func.func @array
  // BACKWARD: reussir.array.view
  // BACKWARD: scf.for
  // BACKWARD: scf.for
  // BACKWARD: reussir.rc.fetch_sub
  // BACKWARD: reussir.rc.defer
  func.func @array(%ref: !reussir.ref<!array unspecified atomic>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!array unspecified atomic>)
    return
  }

  // Opaque cleanup stays pending even though the foreign object is a leaf.
  // FORWARD-LABEL: func.func @foreign
  // FORWARD: reussir.rc.fetch
  // FORWARD: reussir.drop.defer @__reussir_drop.release.forward
  // BACKWARD-LABEL: func.func @foreign
  // BACKWARD: reussir.rc.fetch
  // BACKWARD: reussir.drop.defer @__reussir_drop.release.backward
  func.func @foreign(%ref: !reussir.ref<!foreign>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!foreign>)
    return
  }
}

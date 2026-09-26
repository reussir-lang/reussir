// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s --check-prefixes=DROP,ACQUIRE
// RUN: %reussir-opt %s --reussir-convert-to-std --reussir-acquire-drop-expansion | %FileCheck %s --check-prefix=CLONE --implicit-check-not=reussir.ref.spilled

// Ownership of a dynamic-extent array:
// the element traversal never unrolls a dynamic shape, takes its loop bounds
// from `memref.dim` on the view, and projects through the dynamic strided
// layout at every rank.

!elt = !reussir.rc<i64>
!dv = !reussir.array<? x 2 x !elt>
!rc_dv = !reussir.rc<!dv>
!view = memref<?x2x!elt, strided<[?, ?], offset: ?>>

module {
  // DROP-LABEL: func.func @drop_dynamic(
  // DROP: %[[VIEW:.+]] = reussir.array.view(%arg0 : !reussir.ref<!reussir.array<? x 2 x !reussir.rc<i64>>>) : memref<?x2x!reussir.rc<i64>, strided<[?, ?], offset: ?>>
  // DROP: %[[DIM:.+]] = memref.dim %[[VIEW]], %{{.+}} : memref<?x2x!reussir.rc<i64>, strided<[?, ?], offset: ?>>
  // DROP: scf.for %[[ROW:.+]] = %{{.+}} to %[[DIM]]
  // DROP: scf.for %[[COLUMN:.+]] = %{{.+}} to %{{.+}}
  // DROP: %[[ROW_VIEW:.+]] = reussir.array.project(%[[VIEW]] : memref<?x2x!reussir.rc<i64>, strided<[?, ?], offset: ?>>) [%[[ROW]] : index] : memref<2x!reussir.rc<i64>, strided<[?], offset: ?>>
  // DROP: reussir.array.project(%[[ROW_VIEW]] : memref<2x!reussir.rc<i64>, strided<[?], offset: ?>>) [%[[COLUMN]] : index] : !reussir.ref<!reussir.rc<i64>>
  // DROP: reussir.rc.dec
  // DROP-NOT: reussir.rc.dec
  // DROP: return
  func.func @drop_dynamic(%xs: !reussir.ref<!dv>) {
    reussir.ref.drop (%xs : !reussir.ref<!dv>)
    return
  }

  // ACQUIRE-LABEL: func.func @acquire_dynamic_inner(
  // ACQUIRE: %[[VIEW:.+]] = reussir.array.view
  // ACQUIRE: %[[DIM:.+]] = memref.dim %[[VIEW]], %{{.+}} : memref<2x?x!reussir.rc<i64>, strided<[?, ?], offset: ?>>
  // ACQUIRE: scf.for %[[ROW:.+]] =
  // ACQUIRE: scf.for %[[COLUMN:.+]] = %{{.+}} to %[[DIM]]
  // ACQUIRE: %[[ROW_VIEW:.+]] = reussir.array.project(%[[VIEW]]{{.*}}) [%[[ROW]] : index] : memref<?x!reussir.rc<i64>, strided<[?], offset: ?>>
  // ACQUIRE: reussir.array.project(%[[ROW_VIEW]]{{.*}}) [%[[COLUMN]] : index]
  // ACQUIRE: reussir.rc.inc
  // ACQUIRE-NOT: reussir.rc.inc
  // ACQUIRE: return
  func.func @acquire_dynamic_inner(%xs: !reussir.ref<!reussir.array<2 x ? x !elt>>) {
    reussir.ref.acquire (%xs : !reussir.ref<!reussir.array<2 x ? x !elt>>)
    return
  }

  // Dynamic clones use the same initializer as static clones. The source's
  // logical extents size the allocation and the loop acquires each element.
  // CLONE-LABEL: func.func @clone_dynamic(
  // CLONE: reussir.rc.is_unique
  // CLONE: } else {
  // CLONE: %[[SRC:.+]] = reussir.rc.borrow(%arg0
  // CLONE: %[[SRC_VIEW:.+]] = reussir.array.view(%[[SRC]]
  // CLONE: %[[N:.+]] = memref.dim %[[SRC_VIEW]],
  // CLONE: arith.muli %[[N]],
  // CLONE: arith.muli
  // CLONE: %[[BYTES:.+]] = arith.addi
  // CLONE: %[[TOKEN:.+]] = reussir.token.alloc(%[[BYTES]] : index) : <align : 8, size : ?>
  // CLONE: %[[CLONED:.+]] = reussir.array.instantiate(%[[TOKEN]] : !reussir.token<align : 8, size : ?>) extents(%[[N]])
  // CLONE: scf.for %[[I:.+]] = %{{.+}} to %[[N]]
  // CLONE: scf.for %[[J:.+]] =
  // CLONE: %[[COPY_VIEW:.+]] = reussir.array.view(%[[SRC]]
  // CLONE: %[[SLOT_VIEW:.+]] = memref.subview %[[COPY_VIEW]][%[[I]], %[[J]]]
  // CLONE: %[[SLOT:.+]] = reussir.ref.from_memref(%[[SLOT_VIEW]]
  // CLONE: reussir.ref.load(%[[SLOT]]
  // CLONE: reussir.rc.inc
  // CLONE: %[[ELEMENT:.+]] = memref.load %[[COPY_VIEW]][%[[I]], %[[J]]]
  // CLONE: memref.store %[[ELEMENT]],
  // CLONE: reussir.rc.fetch(%arg0
  // CLONE: reussir.rc.set(%arg0
  // CLONE: scf.yield %[[CLONED]]
  func.func @clone_dynamic(%xs: !rc_dv) -> !rc_dv {
    %res = reussir.array.with_unique_view (%xs : !rc_dv) -> !rc_dv {
      ^bb0(%view: !view):
        reussir.scf.yield
    }
    return %res : !rc_dv
  }
}

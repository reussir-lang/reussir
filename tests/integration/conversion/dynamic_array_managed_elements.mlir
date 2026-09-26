// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s --check-prefixes=DROP,ACQUIRE

// Ownership of a dynamic-extent array:
// the element traversal never unrolls a dynamic shape, takes its loop bounds
// from `memref.dim` on the view, and projects through the dynamic strided
// layout at every rank.

!elt = !reussir.rc<i64>
!dv = !reussir.array<? x 2 x !elt>

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
}

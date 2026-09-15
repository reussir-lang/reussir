// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s --check-prefix=DROP
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,convert-scf-to-cf,reussir-lowering-basic-ops,convert-to-llvm,reconcile-unrealized-casts,cse,canonicalize)' | %FileCheck %s --check-prefix=LLVM

// Ownership of a dynamic-extent array (docs/design/dynamic-extent-arrays.md):
// the element traversal never unrolls a dynamic shape, takes its loop bounds
// from `memref.dim` on the view, and projects through the dynamic strided
// layout at every rank.

!elt = !reussir.rc<i64>
!dv = !reussir.array<? x 2 x !elt>
!view = memref<?x2x!elt, strided<[?, ?], offset: ?>>
!row = memref<2x!elt, strided<[?], offset: ?>>

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

  // A strided projection is descriptor arithmetic: the leading extent for
  // the bounds assumption is the descriptor's size, the row shift lands in
  // the live offset field (`offset + i * stride[0]`), the aligned pointer is
  // untouched, and the trailing size/stride pass through.
  // The bare-argument descriptor: `%arg1` aligned pointer, `%arg2` offset,
  // `%arg3`/`%arg4` sizes, `%arg5`/`%arg6` strides, `%arg7` the index.
  // LLVM-LABEL: llvm.func @project_dynamic(
  // LLVM: %[[IN_BOUNDS:.+]] = llvm.icmp "ult" %arg7, %arg3
  // LLVM: llvm.intr.assume %[[IN_BOUNDS]]
  // LLVM: %[[DELTA:.+]] = llvm.mul %arg7, %arg5
  // LLVM: %[[SHIFT:.+]] = llvm.add %arg2, %[[DELTA]]
  // LLVM: llvm.insertvalue %arg1, %{{.+}}[1]
  // LLVM: llvm.insertvalue %[[SHIFT]], %{{.+}}[2]
  // LLVM: llvm.insertvalue %arg4, %{{.+}}[3, 0]
  // LLVM: llvm.insertvalue %arg6, %{{.+}}[4, 0]
  // LLVM-NOT: llvm.getelementptr
  // LLVM: llvm.return
  func.func @project_dynamic(%view: !view, %i: index) -> !row {
    %row = reussir.array.project(%view : !view) [%i : index] : !row
    return %row : !row
  }
}

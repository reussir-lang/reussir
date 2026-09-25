// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation,reussir-token-reuse))' | %FileCheck %s
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,sccp,canonicalize,cse,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-convert-to-std{expand-arrays=false},func.func(reussir-token-reuse))' -o %t.mlir
// RUN: %FileCheck %s < %t.mlir
// RUN: %reussir-opt %t.mlir --pass-pipeline='builtin.module(reussir-convert-to-std,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts)' -o /dev/null

!array = !reussir.array<? x i32>
!rc = !reussir.rc<!array>
!view = memref<?xi32, strided<[?], offset: ?>>
!matrix = !reussir.array<? x ? x i32>
!rcmatrix = !reussir.rc<!matrix>
!matrixview = memref<?x?xi32, strided<[?, ?], offset: ?>>
module {
  // Neither allocation size is constant; their affine expressions are equal.
  // CHECK-LABEL: func.func @equal_expression
  // CHECK: reussir.token.ensure
  func.func @equal_expression(%n: index) -> !rc {
    %c1 = arith.constant 1 : index
    %sum = arith.addi %n, %c1 : index
    %extent = arith.subi %sum, %c1 : index
    %old = reussir.array.create extents(%n) : !rc
    reussir.rc.dec (%old : !rc)
    %new = reussir.array.create extents(%extent) : !rc
    return %new : !rc
  }

  // The product m*n is nonlinear, but each constructor extent is identical.
  // CHECK-LABEL: func.func @same_matrix_shape
  // CHECK: reussir.token.ensure
  func.func @same_matrix_shape(%m: index, %n: index) -> !rcmatrix {
    %old = reussir.array.create extents(%m, %n) : !rcmatrix
    reussir.rc.dec (%old : !rcmatrix)
    %new = reussir.array.create extents(%m, %n) : !rcmatrix
    return %new : !rcmatrix
  }

  // Incoming arrays have no local allocation operand. Dimensions of their
  // existing view still prove that a replacement has the same footprint.
  // CHECK-LABEL: func.func @copy_shape
  // CHECK: reussir.token.ensure
  // CHECK-SAME: size(
  func.func @copy_shape(%old: !rcmatrix) -> !rcmatrix {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %ref = reussir.rc.borrow(%old : !rcmatrix) : !reussir.ref<!matrix>
    %view = reussir.array.view(%ref : !reussir.ref<!matrix>) : !matrixview
    %m = memref.dim %view, %c0 : !matrixview
    %n = memref.dim %view, %c1 : !matrixview
    reussir.rc.dec (%old : !rcmatrix)
    %new = reussir.array.create extents(%m, %n) : !rcmatrix
    return %new : !rcmatrix
  }

  // Locally constructed arrays can expose their extents through a view too.
  // CHECK-LABEL: func.func @copy_local_shape
  // CHECK: reussir.token.ensure
  func.func @copy_local_shape(%extent: index) -> !rc {
    %c0 = arith.constant 0 : index
    %old = reussir.array.create extents(%extent) : !rc
    %ref = reussir.rc.borrow(%old : !rc) : !reussir.ref<!array>
    %view = reussir.array.view(%ref : !reussir.ref<!array>) : !view
    %n = memref.dim %view, %c0 : !view
    reussir.rc.dec (%old : !rc)
    %new = reussir.array.create extents(%n) : !rc
    return %new : !rc
  }

  // Matching byte expressions can also relate different element types.
  // CHECK-LABEL: func.func @equal_bytes
  // CHECK: reussir.token.ensure
  func.func @equal_bytes(%n: index) -> !reussir.rc<!reussir.array<? x i16>> {
    %c2 = arith.constant 2 : index
    %twice = arith.muli %n, %c2 : index
    %old = reussir.array.create extents(%n) : !rc
    reussir.rc.dec (%old : !rc)
    %new = reussir.array.create extents(%twice) : !reussir.rc<!reussir.array<? x i16>>
    return %new : !reussir.rc<!reussir.array<? x i16>>
  }

  // CHECK-LABEL: func.func @grow
  // CHECK: reussir.token.realloc
  func.func @grow(%old: !rc) -> !rc {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %ref = reussir.rc.borrow(%old : !rc) : !reussir.ref<!array>
    %view = reussir.array.view(%ref : !reussir.ref<!array>) : !view
    %n = memref.dim %view, %c0 : !view
    %extent = arith.addi %n, %c1 : index
    reussir.rc.dec (%old : !rc)
    %new = reussir.array.create extents(%extent) : !rc
    return %new : !rc
  }

  // A dimension from another array is not evidence about the donor.
  // CHECK-LABEL: func.func @unrelated_shape
  // CHECK: reussir.token.realloc
  func.func @unrelated_shape(%old: !rc, %other: !rc) -> !rc {
    %c0 = arith.constant 0 : index
    %ref = reussir.rc.borrow(%other : !rc) : !reussir.ref<!array>
    %view = reussir.array.view(%ref : !reussir.ref<!array>) : !view
    %n = memref.dim %view, %c0 : !view
    reussir.rc.dec (%old : !rc)
    %new = reussir.array.create extents(%n) : !rc
    return %new : !rc
  }
}

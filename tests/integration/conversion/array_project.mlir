// RUN: %reussir-opt %s --reussir-convert-to-std | %FileCheck %s --check-prefix=STD
// RUN: %reussir-opt %s --canonicalize --cse | %FileCheck %s --check-prefix=LIVE
// RUN: %reussir-opt %s --reussir-convert-to-std --canonicalize | %FileCheck %s --check-prefix=FOLD
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-convert-to-std,canonicalize,expand-strided-metadata,lower-affine,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir > %t.ll

module {
  // STD-LABEL: func.func @row
  // STD: %[[WITHIN:.+]] = arith.cmpi ult, %arg1, %{{.+}}
  // STD: %[[NONNEGATIVE:.+]] = arith.cmpi sge, %arg1, %{{.+}}
  // STD: %[[OK:.+]] = arith.andi %[[NONNEGATIVE]], %[[WITHIN]]
  // STD: scf.if %[[OK]]
  // STD: memref.subview %arg0[%arg1, 0, 0] [1, 4, 5] [1, 1, 1]
  // STD-SAME: to memref<4x5xf32, strided<[5, 1], offset: ?>>
  // STD: } else {
  // STD: reussir.panic "array index out of bounds"
  func.func @row(%a: memref<3x4x5xf32>, %i: index) -> memref<4x5xf32, strided<[5, 1], offset: ?>> {
    %r = reussir.array.project(%a : memref<3x4x5xf32>)[%i : index] : memref<4x5xf32, strided<[5, 1], offset: ?>>
    return %r : memref<4x5xf32, strided<[5, 1], offset: ?>>
  }

  // STD-LABEL: func.func @dynamic
  // STD: %[[DIM:.+]] = memref.dim %arg0,
  // STD: arith.cmpi ult, %arg1, %[[DIM]]
  // STD: scf.if
  // STD: memref.subview
  // STD-SAME: to memref<4xf32, strided<[1], offset: ?>>
  func.func @dynamic(%a: memref<?x4xf32>, %i: index) -> memref<4xf32, strided<[1], offset: ?>> {
    %r = reussir.array.project(%a : memref<?x4xf32>)[%i : index] : memref<4xf32, strided<[1], offset: ?>>
    return %r : memref<4xf32, strided<[1], offset: ?>>
  }

  // Even with several unit dimensions, only the leading stride is dropped.
  // STD-LABEL: func.func @unit_dimensions
  // STD: memref.subview %arg0[%arg1, 0, 0] [1, 1, 1] [1, 1, 1]
  // STD-SAME: to memref<1x1xf32, strided<[7, 2], offset: ?>>
  func.func @unit_dimensions(%a: memref<2x1x1xf32, strided<[30, 7, 2], offset: 9>>, %i: index) -> memref<1x1xf32, strided<[7, 2], offset: ?>> {
    %r = reussir.array.project(%a : memref<2x1x1xf32, strided<[30, 7, 2], offset: 9>>)[%i : index] : memref<1x1xf32, strided<[7, 2], offset: ?>>
    return %r : memref<1x1xf32, strided<[7, 2], offset: ?>>
  }

  // STD-LABEL: func.func @element
  // STD: scf.if
  // STD: %[[SUB:.+]] = memref.subview
  // STD-SAME: to memref<i32, strided<[], offset: ?>>
  // STD: reussir.ref.from_memref(%[[SUB]]
  // STD: } else {
  // STD: reussir.panic
  func.func @element(%a: memref<?xi32, strided<[2], offset: 3>>, %i: index) -> !reussir.ref<i32 field> {
    %r = reussir.array.project(%a : memref<?xi32, strided<[2], offset: 3>>)[%i : index] : !reussir.ref<i32 field>
    return %r : !reussir.ref<i32 field>
  }

  // An unused result must not remove a failing bounds check before lowering.
  // LIVE-LABEL: func.func @unused_negative
  // LIVE: reussir.array.project
  // FOLD-LABEL: func.func @unused_negative
  // FOLD-NOT: memref.subview
  // FOLD: reussir.panic "array index out of bounds"
  func.func @unused_negative(%a: memref<4xi32>) {
    %i = arith.constant -1 : index
    %r = reussir.array.project(%a : memref<4xi32>)[%i : index] : !reussir.ref<i32 field>
    return
  }

  // A negative constant also fails with an unknown leading extent. Keep the
  // result live so dead-result elimination cannot hide bad offset expansion.
  // FOLD-LABEL: func.func @negative_dynamic
  // FOLD-NOT: memref.subview
  // FOLD: reussir.panic "array index out of bounds"
  func.func @negative_dynamic(%a: memref<?xi32>) -> !reussir.ref<i32 field> {
    %i = arith.constant -2 : index
    %negative = arith.addi %i, %i : index
    %r = reussir.array.project(%a : memref<?xi32>)[%negative : index] : !reussir.ref<i32 field>
    return %r : !reussir.ref<i32 field>
  }

  // LIVE-LABEL: func.func @unused_end
  // LIVE: reussir.array.project
  // FOLD-LABEL: func.func @unused_end
  // FOLD-NOT: memref.subview
  // FOLD: reussir.panic "array index out of bounds"
  func.func @unused_end(%a: memref<4xi32>) {
    %i = arith.constant 4 : index
    %r = reussir.array.project(%a : memref<4xi32>)[%i : index] : !reussir.ref<i32 field>
    return
  }

  // FOLD-LABEL: func.func @known_valid
  // FOLD-NOT: scf.if
  // FOLD-NOT: reussir.panic
  // FOLD: return
  func.func @known_valid(%a: memref<4xi32>) -> !reussir.ref<i32 field> {
    %i = arith.constant 2 : index
    %r = reussir.array.project(%a : memref<4xi32>)[%i : index] : !reussir.ref<i32 field>
    return %r : !reussir.ref<i32 field>
  }
}

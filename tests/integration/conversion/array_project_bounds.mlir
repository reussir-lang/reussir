// RUN: %reussir-opt %s --reussir-convert-to-std | %FileCheck %s --implicit-check-not=reussir.array.project

module {
  func.func private @consume(!reussir.ref<i32>)

  // Prove a nonconstant index from both arms of arith.select.
  // CHECK-LABEL: func.func @selected_in_bounds
  // CHECK-NOT: scf.if
  // CHECK: memref.subview
  // CHECK-NOT: reussir.panic
  // CHECK: return
  func.func @selected_in_bounds(%a: memref<4xi32>, %choose: i1) -> !reussir.ref<i32> {
    %zero = arith.constant 0 : index
    %three = arith.constant 3 : index
    %i = arith.select %choose, %zero, %three : index
    %r = reussir.array.project(%a : memref<4xi32>)[%i : index] : !reussir.ref<i32>
    return %r : !reussir.ref<i32>
  }

  // The induction variable is in bounds even when the dimension is dynamic.
  // No canonicalizer is run: conversion itself must omit the guard.
  // CHECK-LABEL: func.func @dynamic_loop
  // CHECK: scf.for
  // CHECK-NOT: scf.if
  // CHECK: memref.subview
  // CHECK-NOT: reussir.panic
  // CHECK: return
  func.func @dynamic_loop(%a: memref<?xi32>) {
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    %n = memref.dim %a, %zero : memref<?xi32>
    scf.for %i = %zero to %n step %one {
      %r = reussir.array.project(%a : memref<?xi32>)[%i : index] : !reussir.ref<i32>
      func.call @consume(%r) : (!reussir.ref<i32>) -> ()
    }
    return
  }

  // Bounds survive rank reduction through a subview, as in array traversal.
  // CHECK-LABEL: func.func @nested_dynamic_loop
  // CHECK: scf.for
  // CHECK-NOT: scf.if
  // CHECK: memref.subview
  // CHECK: scf.for
  // CHECK-NOT: scf.if
  // CHECK: memref.subview
  // CHECK-NOT: reussir.panic
  // CHECK: return
  func.func @nested_dynamic_loop(%a: memref<?x?xi32>) {
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    %rows = memref.dim %a, %zero : memref<?x?xi32>
    %cols = memref.dim %a, %one : memref<?x?xi32>
    scf.for %i = %zero to %rows step %one {
      %row = reussir.array.project(%a : memref<?x?xi32>)[%i : index] : memref<?xi32, strided<[1], offset: ?>>
      scf.for %j = %zero to %cols step %one {
        %r = reussir.array.project(%row : memref<?xi32, strided<[1], offset: ?>>)[%j : index] : !reussir.ref<i32>
        func.call @consume(%r) : (!reussir.ref<i32>) -> ()
      }
    }
    return
  }

  // Proving only the upper bound must not remove the negative-index check.
  // CHECK-LABEL: func.func @negative_start
  // CHECK: scf.for
  // CHECK: scf.if
  // CHECK: reussir.panic
  // CHECK: return
  func.func @negative_start(%a: memref<?xi32>) {
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    %negative = arith.constant -1 : index
    %n = memref.dim %a, %zero : memref<?xi32>
    scf.for %i = %negative to %n step %one {
      %r = reussir.array.project(%a : memref<?xi32>)[%i : index] : !reussir.ref<i32>
      func.call @consume(%r) : (!reussir.ref<i32>) -> ()
    }
    return
  }

  // Likewise a nonnegative index may reach the exclusive upper bound.
  // CHECK-LABEL: func.func @past_end
  // CHECK: scf.for
  // CHECK: scf.if
  // CHECK: reussir.panic
  // CHECK: return
  func.func @past_end(%a: memref<?xi32>) {
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    %n = memref.dim %a, %zero : memref<?xi32>
    scf.for %i = %zero to %n step %one {
      %next = arith.addi %i, %one : index
      %r = reussir.array.project(%a : memref<?xi32>)[%next : index] : !reussir.ref<i32>
      func.call @consume(%r) : (!reussir.ref<i32>) -> ()
    }
    return
  }
}

// RUN: %reussir-opt %s --loop-invariant-code-motion | %FileCheck %s

module {
  func.func private @consume(!reussir.ref<i32 field>)

  // CHECK-LABEL: func.func @in_bounds
  // CHECK: reussir.array.project
  // CHECK: scf.for
  func.func @in_bounds(%view: memref<4xi32>, %n: index) {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %c3 = arith.constant 3 : index
    scf.for %i = %c0 to %n step %c1 {
      %ref = reussir.array.project(%view : memref<4xi32>)[%c3 : index] : !reussir.ref<i32 field>
      func.call @consume(%ref) : (!reussir.ref<i32 field>) -> ()
    }
    return
  }

  // Upstream value-bounds reasoning also proves computed indices in range;
  // requiring a literal constant would unnecessarily block this hoist.
  // CHECK-LABEL: func.func @selected_index
  // CHECK: reussir.array.project
  // CHECK: scf.for
  func.func @selected_index(%view: memref<4xi32>, %choose: i1, %n: index) {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %c3 = arith.constant 3 : index
    %bounded = arith.select %choose, %c0, %c3 : index
    scf.for %i = %c0 to %n step %c1 {
      %ref = reussir.array.project(%view : memref<4xi32>)[%bounded : index] : !reussir.ref<i32 field>
      func.call @consume(%ref) : (!reussir.ref<i32 field>) -> ()
    }
    return
  }

  // Each branch of a computed index must satisfy both array bounds.
  // CHECK-LABEL: func.func @selected_maybe_out_of_bounds
  // CHECK: scf.for
  // CHECK-COUNT-2: reussir.array.project
  func.func @selected_maybe_out_of_bounds(%view: memref<4xi32>, %choose: i1, %n: index) {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %c3 = arith.constant 3 : index
    %c4 = arith.constant 4 : index
    %negative = arith.constant -1 : index
    %maybe_past = arith.select %choose, %c0, %c4 : index
    %maybe_before = arith.select %choose, %negative, %c3 : index
    scf.for %i = %c0 to %n step %c1 {
      %end = reussir.array.project(%view : memref<4xi32>)[%maybe_past : index] : !reussir.ref<i32 field>
      %before = reussir.array.project(%view : memref<4xi32>)[%maybe_before : index] : !reussir.ref<i32 field>
      func.call @consume(%end) : (!reussir.ref<i32 field>) -> ()
      func.call @consume(%before) : (!reussir.ref<i32 field>) -> ()
    }
    return
  }
}

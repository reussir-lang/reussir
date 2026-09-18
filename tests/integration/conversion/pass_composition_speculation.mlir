// RUN: %reussir-opt %s --loop-invariant-code-motion | %FileCheck %s

// Projection assumes an in-bounds index during LLVM lowering. Unknown and
// out-of-bounds indices must stay under their original execution guard.
module {
  func.func private @consume(!reussir.ref<i32 field>)

  // CHECK-LABEL: func.func @unknown_index
  // CHECK: scf.for
  // CHECK: reussir.array.project
  func.func @unknown_index(%view: memref<4xi32>, %index: index, %n: index) {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    scf.for %i = %c0 to %n step %c1 {
      %ref = reussir.array.project(%view : memref<4xi32>)[%index : index] : !reussir.ref<i32 field>
      func.call @consume(%ref) : (!reussir.ref<i32 field>) -> ()
    }
    return
  }

  // The loop may be empty: speculating either assumption would then turn a
  // defined program into UB, even if the projected pointer is never loaded.
  // CHECK-LABEL: func.func @out_of_bounds
  // CHECK: scf.for
  // CHECK-COUNT-2: reussir.array.project
  func.func @out_of_bounds(%view: memref<4xi32>, %n: index) {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %c4 = arith.constant 4 : index
    %negative = arith.constant -1 : index
    scf.for %i = %c0 to %n step %c1 {
      %end = reussir.array.project(%view : memref<4xi32>)[%c4 : index] : !reussir.ref<i32 field>
      %before = reussir.array.project(%view : memref<4xi32>)[%negative : index] : !reussir.ref<i32 field>
      func.call @consume(%end) : (!reussir.ref<i32 field>) -> ()
      func.call @consume(%before) : (!reussir.ref<i32 field>) -> ()
    }
    return
  }

  // A pointer-test dispatch with a pure body can move out of the loop; its
  // region captures must still obey ordinary LICM operand availability.
  // CHECK-LABEL: func.func @pure_dispatch
  // CHECK: reussir.nullable.dispatch
  // CHECK: scf.for
  func.func @pure_dispatch(%ptr: !reussir.nullable<!reussir.ref<i32>>, %n: index) -> i32 {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %zero = arith.constant 0 : i32
    %one = arith.constant 1 : i32
    %r = scf.for %i = %c0 to %n step %c1 iter_args(%acc = %zero) -> i32 {
      %v = reussir.nullable.dispatch(%ptr : !reussir.nullable<!reussir.ref<i32>>) -> i32 {
        nonnull -> {
        ^bb0(%ref: !reussir.ref<i32>):
          reussir.scf.yield %one : i32
        }
        null -> { reussir.scf.yield %zero : i32 }
      }
      %next = arith.addi %acc, %v : i32
      scf.yield %next : i32
    }
    return %r : i32
  }

}

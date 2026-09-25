// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

module {
  func.func @lost_static_stride(%view: memref<?x2xi32>, %i: index) {
    // expected-error @+1 {{projected subview type mismatch: expected}}
    %row = reussir.array.project(%view : memref<?x2xi32>) [%i : index] : memref<2xi32>
    return
  }
}

// -----

module {
  func.func @lost_dynamic_layout(%view: memref<?x2xi32, strided<[?, ?], offset: ?>>, %i: index) {
    // A static trailing shape still carries the source's runtime stride.
    // expected-error @+1 {{projected subview type mismatch: expected}}
    %row = reussir.array.project(%view : memref<?x2xi32, strided<[?, ?], offset: ?>>) [%i : index] : memref<2xi32>
    return
  }
}

// RUN: %not %reussir-opt %s 2>&1 | %FileCheck %s

// CHECK: array elements must have a fixed size
module {
  func.func @scalable(%n: index, %value: vector<[4]xi32>) -> !reussir.rc<!reussir.array<? x vector<[4]xi32>>> {
    %rc = reussir.array.create extents(%n) : !reussir.rc<!reussir.array<? x vector<[4]xi32>>> body {
      ^bb0(%array_i0: index):
        reussir.scf.yield %value : vector<[4]xi32>
    }
    return %rc : !reussir.rc<!reussir.array<? x vector<[4]xi32>>>
  }
}

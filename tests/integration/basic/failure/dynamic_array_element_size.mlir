// RUN: %not %reussir-opt %s 2>&1 | %FileCheck %s

// CHECK: array elements must have a fixed size
module {
  func.func @scalable(%n: index, %value: vector<[4]xi32>) -> !reussir.rc<!reussir.array<? x vector<[4]xi32>>> {
    %rc = reussir.array.create init(%value : vector<[4]xi32>) extents(%n) : !reussir.rc<!reussir.array<? x vector<[4]xi32>>>
    return %rc : !reussir.rc<!reussir.array<? x vector<[4]xi32>>>
  }
}

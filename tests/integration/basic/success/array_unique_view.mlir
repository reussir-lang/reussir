// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s

!arr4 = !reussir.array<4 x i8>
!rc_arr4 = !reussir.rc<!arr4>

module {
  func.func @roundtrip(%xs: !rc_arr4) -> !rc_arr4 {
    %res = reussir.array.with_unique_view (%xs : !rc_arr4) -> !rc_arr4 {
      ^bb0(%view: memref<4xi8>):
        %c0 = arith.constant 0 : index
        %v = memref.load %view[%c0] : memref<4xi8>
        reussir.scf.yield
    }
    return %res : !rc_arr4
  }
}

// CHECK-LABEL: module {
// CHECK: %[[RES:.+]] = reussir.array.with_unique_view
// CHECK: ^bb0(%[[VIEW:.+]]: memref<4xi8>)
// CHECK: memref.load %[[VIEW]]
// CHECK: reussir.scf.yield

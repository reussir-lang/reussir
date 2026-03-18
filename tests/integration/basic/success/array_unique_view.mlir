// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s

!arr4 = !reussir.array<4 x i8>
!rc_arr4 = !reussir.rc<!arr4>

module {
  func.func @roundtrip(%xs: !rc_arr4) -> !rc_arr4 {
    %res = reussir.array.with_unique_view (%xs : !rc_arr4) -> !rc_arr4 {
      ^bb0(%view: !reussir.view<mutable, 4 x i8>):
        %c0 = arith.constant 0 : index
        %elt = reussir.array.project (%view : !reussir.view<mutable, 4 x i8>) [%c0 : index] : !reussir.ref<i8 field>
        %v = reussir.ref.load (%elt : !reussir.ref<i8 field>) : i8
        reussir.scf.yield
    }
    return %res : !rc_arr4
  }
}

// CHECK-LABEL: module {
// CHECK: %[[RES:.+]] = reussir.array.with_unique_view
// CHECK: ^bb0(%[[VIEW:.+]]: !reussir.view<mutable, 4 x i8>)
// CHECK: %[[ELT:.+]] = reussir.array.project
// CHECK: reussir.scf.yield

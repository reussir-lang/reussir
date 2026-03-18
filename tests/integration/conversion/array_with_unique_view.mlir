// RUN: %reussir-opt %s --reussir-lowering-scf-ops | %FileCheck %s

!arr4 = !reussir.array<4 x i8>
!rc_arr4 = !reussir.rc<!arr4>

module {
  func.func @update0(%xs: !rc_arr4) -> !rc_arr4 {
    %res = reussir.array.with_unique_view (%xs : !rc_arr4) -> !rc_arr4 {
      ^bb0(%view: !reussir.view<mutable, 4 x i8>):
        %c0 = arith.constant 0 : index
        %elt = reussir.array.project (%view : !reussir.view<mutable, 4 x i8>) [%c0 : index] : !reussir.ref<i8 field>
        %old = reussir.ref.load (%elt : !reussir.ref<i8 field>) : i8
        %one = arith.constant 1 : i8
        %new = arith.addi %old, %one : i8
        reussir.ref.store (%elt : !reussir.ref<i8 field>) (%new : i8)
        reussir.scf.yield
    }
    return %res : !rc_arr4
  }
}

// CHECK-LABEL: func.func @update0(
// CHECK: %[[IS_UNIQUE:.+]] = reussir.rc.is_unique
// CHECK: %[[RESULT:.+]] = scf.if %[[IS_UNIQUE]] -> (!reussir.rc<!reussir.array<4 x i8>>) {
// CHECK: %[[BORROWED:.+]] = reussir.rc.borrow
// CHECK: %[[VIEW:.+]] = reussir.array.view(%[[BORROWED]] : !reussir.ref<!reussir.array<4 x i8>>) : !reussir.view<mutable, 4 x i8>
// CHECK: reussir.array.project
// CHECK: scf.yield %arg0 : !reussir.rc<!reussir.array<4 x i8>>
// CHECK: } else {
// CHECK: %[[LOAD_BORROW:.+]] = reussir.rc.borrow
// CHECK: %[[PAYLOAD:.+]] = reussir.ref.load
// CHECK: %[[TOKEN:.+]] = reussir.token.alloc
// CHECK: %[[CLONED:.+]] = reussir.rc.create
// CHECK: reussir.rc.dec
// CHECK: %[[CLONED_BORROW:.+]] = reussir.rc.borrow
// CHECK: %[[CLONED_VIEW:.+]] = reussir.array.view(%[[CLONED_BORROW]] : !reussir.ref<!reussir.array<4 x i8>>) : !reussir.view<mutable, 4 x i8>
// CHECK: reussir.array.project
// CHECK: scf.yield %[[CLONED]] : !reussir.rc<!reussir.array<4 x i8>>
// CHECK: }
// CHECK: return %[[RESULT]] : !reussir.rc<!reussir.array<4 x i8>>

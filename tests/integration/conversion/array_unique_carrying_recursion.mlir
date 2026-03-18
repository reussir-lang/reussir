// RUN: %reussir-opt %s --reussir-unique-carrying-recursion-analysis | %FileCheck %s
// RUN: %reussir-opt %s \
// RUN:   --reussir-unique-carrying-recursion-analysis \
// RUN:   --reussir-lowering-scf-ops \
// RUN:   --reussir-rc-decrement-expansion \
// RUN:   --reussir-acquire-drop-expansion \
// RUN:   --reussir-acquire-drop-expansion='expand-decrement=1 outline-record=1' \
// RUN:   --convert-scf-to-cf \
// RUN:   --reussir-lowering-basic-ops | \
// RUN:   %reussir-translate --mlir-to-llvmir | \
// RUN:   %opt -passes='default<O3>' -pass-remarks=loop-vectorize -disable-output 2>&1 | \
// RUN:   %FileCheck %s --check-prefix=VEC

!arr512 = !reussir.array<512 x i8>
!rc_arr512 = !reussir.rc<!arr512>

module {
  // CHECK-LABEL: func.func private @inc_1(
  // CHECK: attributes {llvm.linkage = #llvm.linkage<internal>, reussir.carrying_uniqueness}
  // CHECK: %[[UPDATED:.+]] = reussir.array.with_unique_view
  // CHECK: func.call @inc_1.unique(%[[UPDATED]], %{{.+}}) : (!reussir.rc<!reussir.array<512 x i8>>, index) -> !reussir.rc<!reussir.array<512 x i8>>
  func.func private @inc_1(%xs: !rc_arr512, %i: index) -> !rc_arr512 attributes {llvm.linkage = #llvm.linkage<internal>} {
    %c512 = arith.constant 512 : index
    %cond = arith.cmpi ult, %i, %c512 : index
    %result = scf.if %cond -> (!rc_arr512) {
      %updated = reussir.array.with_unique_view (%xs : !rc_arr512) -> !rc_arr512 {
        ^bb0(%view: !reussir.view<mutable, 512 x i8>):
          %elt = reussir.array.project (%view : !reussir.view<mutable, 512 x i8>) [%i : index] : !reussir.ref<i8 field>
          %old = reussir.ref.load (%elt : !reussir.ref<i8 field>) : i8
          %one = arith.constant 1 : i8
          %new = arith.addi %old, %one : i8
          reussir.ref.store (%elt : !reussir.ref<i8 field>) (%new : i8)
          reussir.scf.yield
      }
      %c1 = arith.constant 1 : index
      %next = arith.addi %i, %c1 : index
      %rec = func.call @inc_1(%updated, %next) : (!rc_arr512, index) -> !rc_arr512
      scf.yield %rec : !rc_arr512
    } else {
      scf.yield %xs : !rc_arr512
    }
    return %result : !rc_arr512
  }

  // CHECK-LABEL: func.func @inc_1_export(
  // CHECK: %[[C0:.+]] = arith.constant 0 : index
  // CHECK: %[[RES:.+]] = call @inc_1(%arg0, %[[C0]]) : (!reussir.rc<!reussir.array<512 x i8>>, index) -> !reussir.rc<!reussir.array<512 x i8>>
  // CHECK: return %[[RES]] : !reussir.rc<!reussir.array<512 x i8>>
  func.func @inc_1_export(%xs: !rc_arr512) -> !rc_arr512 {
    %c0 = arith.constant 0 : index
    %res = func.call @inc_1(%xs, %c0) : (!rc_arr512, index) -> !rc_arr512
    return %res : !rc_arr512
  }

  // CHECK-LABEL: func.func private @inc_1.unique(
  // CHECK: reussir.rc.assume_unique(%arg0 : !reussir.rc<!reussir.array<512 x i8>>)
  // CHECK: %[[UPDATED2:.+]] = reussir.array.with_unique_view
  // CHECK: func.call @inc_1.unique(%[[UPDATED2]], %{{.+}}) : (!reussir.rc<!reussir.array<512 x i8>>, index) -> !reussir.rc<!reussir.array<512 x i8>>

  // VEC: remark: <unknown>:0:0: vectorized loop
}

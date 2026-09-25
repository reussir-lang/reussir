// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-convert-to-std{expand-arrays=false},func.func(reussir-token-reuse))' -o %t.mlir
// RUN: %FileCheck %s < %t.mlir
// RUN: %reussir-opt %t.mlir --pass-pipeline='builtin.module(reussir-convert-to-std,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts)' -o %t.llvm.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.llvm.mlir -o %t.ll

!rc = !reussir.rc<!reussir.array<? x i32>>
!wide = !reussir.rc<!reussir.array<? x i128>>
module {
  // Equal dynamic token types do not imply equal runtime sizes.
  // CHECK-LABEL: func.func @dynamic_to_dynamic
  // CHECK: %[[BYTES:.+]] = arith.addi
  // CHECK-NOT: reussir.token.alloc
  // CHECK: %[[REUSED:.+]] = reussir.token.realloc
  // CHECK-SAME: size(%[[BYTES]] : index)
  // CHECK: reussir.array.create
  // CHECK-SAME: token(%[[REUSED]]
  func.func @dynamic_to_dynamic(%old: !rc, %n: index) -> !rc {
    reussir.rc.dec (%old : !rc)
    %new = reussir.array.create extents(%n) : !rc
    return %new : !rc
  }

  // Different alignments still resize through the allocator.
  // CHECK-LABEL: func.func @different_alignment
  // CHECK: %[[BYTES:.+]] = arith.addi
  // CHECK-NOT: reussir.token.alloc
  // CHECK: %[[REUSED:.+]] = reussir.token.realloc
  // CHECK-SAME: size(%[[BYTES]] : index)
  // CHECK: reussir.array.create
  // CHECK-SAME: token(%[[REUSED]]
  func.func @different_alignment(%old: !wide, %n: index) -> !rc {
    reussir.rc.dec (%old : !wide)
    %new = reussir.array.create extents(%n) : !rc
    return %new : !rc
  }

  // A static donor can also be resized to a runtime byte count.
  // CHECK-LABEL: func.func @static_to_dynamic
  // CHECK: %[[BYTES:.+]] = arith.addi
  // CHECK-NOT: reussir.token.alloc
  // CHECK: %[[REUSED:.+]] = reussir.token.realloc
  // CHECK-SAME: size(%[[BYTES]] : index)
  // CHECK: reussir.array.create
  // CHECK-SAME: token(%[[REUSED]]
  func.func @static_to_dynamic(%old: !reussir.rc<i32>, %n: index) -> !rc {
    reussir.rc.dec (%old : !reussir.rc<i32>)
    %new = reussir.array.create extents(%n) : !rc
    return %new : !rc
  }

  // Dynamic donors continue to support static recipients.
  // CHECK-LABEL: func.func @dynamic_to_static
  // CHECK: %[[REUSED:.+]] = reussir.token.realloc
  // CHECK: reussir.rc.create
  // CHECK-SAME: token(%[[REUSED]]
  func.func @dynamic_to_static(%old: !rc, %value: i32) -> !reussir.rc<i32> {
    reussir.rc.dec (%old : !rc)
    %new = reussir.rc.create value(%value : i32) : !reussir.rc<i32>
    return %new : !reussir.rc<i32>
  }
}

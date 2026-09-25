// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation,reussir-token-reuse))' | %FileCheck %s
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,sccp,canonicalize,cse,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-convert-to-std{expand-arrays=false},func.func(reussir-token-reuse))' -o %t.mlir
// RUN: %FileCheck %s < %t.mlir
// RUN: %reussir-opt %t.mlir --pass-pipeline='builtin.module(reussir-convert-to-std,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts)' -o /dev/null

!rc = !reussir.rc<!reussir.array<? x i32>>
!fixed = !reussir.rc<!reussir.array<7 x i64>>
module {
  // The dynamic box needs 32 + 4 * 8 = 64 bytes, like the static donor.
  // ValueBounds proves (n + 8) - n == 8 without running canonicalization.
  // CHECK-LABEL: func.func @constant_extent
  // CHECK: %[[REUSED:.+]] = reussir.token.ensure
  // CHECK-SAME: size(
  // CHECK: reussir.array.create
  // CHECK-SAME: token(%[[REUSED]]
  func.func @constant_extent(%old: !fixed, %n: index) -> !rc {
    %c8 = arith.constant 8 : index
    %sum = arith.addi %n, %c8 : index
    %extent = arith.subi %sum, %n : index
    reussir.rc.dec (%old : !fixed)
    %new = reussir.array.create extents(%extent) : !rc
    return %new : !rc
  }

  // A range whose upper bound is eight is not an exact size proof.
  // CHECK-LABEL: func.func @bounded_extent
  // CHECK: reussir.token.realloc
  func.func @bounded_extent(%old: !fixed, %n: index) -> !rc {
    %c8 = arith.constant 8 : index
    %extent = arith.minui %n, %c8 : index
    reussir.rc.dec (%old : !fixed)
    %new = reussir.array.create extents(%extent) : !rc
    return %new : !rc
  }

  // Matching byte counts do not erase the alignment requirement.
  // CHECK-LABEL: func.func @different_alignment
  // CHECK: reussir.token.realloc
  func.func @different_alignment(%old: !reussir.rc<!reussir.array<3 x i128>>) -> !rc {
    %c8 = arith.constant 8 : index
    reussir.rc.dec (%old : !reussir.rc<!reussir.array<3 x i128>>)
    %new = reussir.array.create extents(%c8) : !rc
    return %new : !rc
  }

  // A donor created locally retains its allocation size as an SSA value.
  // CHECK-LABEL: func.func @dynamic_donor
  // CHECK: reussir.token.ensure
  func.func @dynamic_donor() -> !fixed {
    %c8 = arith.constant 8 : index
    %old = reussir.array.create extents(%c8) : !rc
    reussir.rc.dec (%old : !rc)
    %new = reussir.array.create extents() : !fixed
    return %new : !fixed
  }
}

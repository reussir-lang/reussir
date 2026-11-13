// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-lowering-basic-ops,reussir-convert-to-llvm)' | %FileCheck %s

// Addresses have the index width of the pointer, not its size.

!P = !ptr.ptr<#ptr.generic_space>

module attributes {
  llvm.data_layout = "e-p:64:64:64:32",
  dlti.dl_spec = #dlti.dl_spec<!llvm.ptr = dense<[64, 64, 64, 32]> : vector<4xi64>>
} {
  // CHECK-LABEL: llvm.func @addresses(
  // CHECK-SAME: %[[P:.*]]: !llvm.ptr, %[[M:.*]]: i64)
  // CHECK: %[[ADDR:.*]] = llvm.ptrtoaddr %[[P]] : !llvm.ptr to i32
  // CHECK: llvm.zext %[[ADDR]] : i32 to i64
  // CHECK: %[[MASK:.*]] = llvm.trunc %[[M]] : i64 to i32
  // CHECK: llvm.intr.ptrmask %[[P]], %[[MASK]] : (!llvm.ptr, i32) -> !llvm.ptr
  func.func @addresses(%p: !P, %mask: i64) -> (i64, !P) {
    %addr = reussir.ptr.addr %p : !P -> i64
    %masked = reussir.ptr.mask %p, %mask : !P, i64
    return %addr, %masked : i64, !P
  }

  // CHECK-LABEL: llvm.func @difference(
  // CHECK: %[[LHS:.*]] = llvm.ptrtoaddr %{{.*}} : !llvm.ptr to i32
  // CHECK: %[[RHS:.*]] = llvm.ptrtoaddr %{{.*}} : !llvm.ptr to i32
  // CHECK: %[[DIFF:.*]] = llvm.sub %[[LHS]], %[[RHS]] : i32
  // CHECK: llvm.sext %[[DIFF]] : i32 to i64
  func.func @difference(%p: !P, %q: !P) -> i64 {
    %d = ptr.ptr_diff %p, %q : !P -> i64
    return %d : i64
  }
}

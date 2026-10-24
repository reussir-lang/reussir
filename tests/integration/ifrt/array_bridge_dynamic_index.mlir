// REQUIRES: openxla
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-convert-to-llvm | %FileCheck %s

#s = #ifrt.sharding_unspecified
!I = !ifrt.array<tensor<?xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0]>>>
!MI = !ifrt.array<tensor<?xf32>, #ifrt.sharding_param<2 to [0] on 2>, [0, 1]>
!MR = !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0, 1], sharding = #ifrt.sharding_param<2 to [0] on 2>>>>
module attributes {
  dlti.dl_spec = #dlti.dl_spec<
    !llvm.ptr = dense<32> : vector<4xi64>, index = 32 : i32,
    i64 = dense<64> : vector<2xi64>, i32 = dense<32> : vector<2xi64>>,
  llvm.data_layout = "e-m:e-p:32:32-i64:64-i128:128-n32:64-S128",
  llvm.target_triple = "wasm32-unknown-unknown"
} {
  // CHECK-LABEL: llvm.func @narrow(
  // CHECK: llvm.call @__reussir_pjrt_launch({{.*}}) : (!llvm.ptr) -> ()
  // CHECK: llvm.call @__reussir_pjrt_array_dimensions({{.*}}) : (!llvm.ptr, !llvm.ptr, i32) -> ()
  // CHECK: %[[SIZE:.*]] = llvm.load {{.*}} : !llvm.ptr -> i64
  // CHECK: %[[NARROW:.*]] = llvm.trunc %[[SIZE]] : i64 to i32
  // CHECK: %[[RESTORED:.*]] = llvm.sext %[[NARROW]] : i32 to i64
  // CHECK: llvm.icmp "eq" %[[RESTORED]], %[[SIZE]]
  // CHECK: llvm.cond_br
  // CHECK: llvm.getelementptr {{.*}}[0, 1, 2]
  // CHECK: llvm.store %[[NARROW]],
  func.func @narrow(%input: !R, %token: !reussir.token<align : 4, size : 16>) -> !R attributes {ifrt.function} {
    %view = reussir.array.to_ifrt %input : !R -> !I
    %out, %done = ifrt.Call @kernel::@main(%view) on devices [0] : (!I) -> !I
    %result = reussir.array.from_ifrt %out token(%token : !reussir.token<align : 4, size : 16>) : !I -> !R
    return %result : !R
  }
  // CHECK-LABEL: llvm.func @explicit(
  // CHECK: llvm.sext {{.*}} : i32 to i64
  // CHECK: llvm.srem
  // CHECK: llvm.sdiv
  // CHECK: llvm.add
  // CHECK: llvm.icmp "eq"
  // CHECK: llvm.trunc {{.*}} : i64 to i32
  func.func @explicit(%input: !MR, %size: index, %token: !reussir.token<align : 4, size : 20>) -> !MR attributes {ifrt.function} {
    %view = reussir.array.to_ifrt %input : !MR -> !MI
    %out, %done = ifrt.Call @kernel::@main(%view) on devices [0, 1] : (!MI) -> !MI
    %result = reussir.array.from_ifrt %out extents(%size) token(%token : !reussir.token<align : 4, size : 20>) : !MI -> !MR
    return %result : !MR
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg: tensor<?xf32>) -> tensor<?xf32> {
      return %arg : tensor<?xf32>
    }
  }
}

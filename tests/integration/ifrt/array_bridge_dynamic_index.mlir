// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-convert-to-llvm | %FileCheck %s

#s = #ifrt.sharding_unspecified
!I = !ifrt.array<tensor<?xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0]>>>
module attributes {
  dlti.dl_spec = #dlti.dl_spec<
    !llvm.ptr = dense<32> : vector<4xi64>, index = 32 : i32,
    i64 = dense<64> : vector<2xi64>, i32 = dense<32> : vector<2xi64>>,
  llvm.data_layout = "e-m:e-p:32:32-i64:64-i128:128-n32:64-S128",
  llvm.target_triple = "wasm32-unknown-unknown"
} {
  // CHECK-LABEL: llvm.func @narrow(
  // CHECK: llvm.call @__reussir_pjrt_compile({{.*}}) : (!llvm.ptr, i32, !llvm.ptr, !llvm.ptr, i32) -> !llvm.ptr
  // CHECK: llvm.call @__reussir_pjrt_executable_validate_devices({{.*}}) : (!llvm.ptr, !llvm.ptr, i32) -> ()
  // CHECK: llvm.call @__reussir_pjrt_executable_execute({{.*}}) : (!llvm.ptr, !llvm.ptr, i32, !llvm.ptr, i32) -> ()
  // CHECK: llvm.call @__reussir_pjrt_array_dimensions({{.*}}) : (!llvm.ptr, !llvm.ptr, i32) -> ()
  // CHECK: %[[SIZE:.*]] = llvm.extractvalue {{.*}}[0] : !llvm.struct<(i64, i1)>
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
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg: tensor<?xf32>) -> tensor<?xf32> {
      return %arg : tensor<?xf32>
    }
  }
}

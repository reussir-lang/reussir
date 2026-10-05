// REQUIRES: ifrt
// RUN: %not %reussir-opt %s --split-input-file --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-convert-to-llvm --mlir-print-op-on-diagnostic=false 2>&1 | %FileCheck %s

#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #s>>>
module @kernel attributes {sym_visibility = "private"} {
  func.func @main() -> tensor<8xf32> {
    %out = stablehlo.constant dense<0.0> : tensor<8xf32>
    return %out : tensor<8xf32>
  }
}
func.func @missing_token() -> !R attributes {ifrt.function} {
  %out, %done = ifrt.Call @kernel::@main() on devices [0] : () -> !I
  // CHECK: 'reussir.array.from_ifrt' op requires an allocation token; run token instantiation first
  %result = reussir.array.from_ifrt %out : !I -> !R
  return %result : !R
}
// -----
!I = !ifrt.array<tensor<?xf32>, #ifrt.sharding_unspecified, [0, 1]>
!R = !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0, 1]>>>
module @kernel attributes {sym_visibility = "private"} {
  func.func @main() -> tensor<?xf32> {
    %out = stablehlo.custom_call @test() : () -> tensor<?xf32>
    return %out : tensor<?xf32>
  }
}
func.func @unresolved_sharding(%token: !reussir.token<align : 8, size : 40>) -> !R attributes {ifrt.function} {
  %out, %done = ifrt.Call @kernel::@main() on devices [0, 1] : () -> !I
  // CHECK: multi-device allocation requires concrete sharding
  %result = reussir.array.from_ifrt %out token(%token : !reussir.token<align : 8, size : 40>) : !I -> !R
  return %result : !R
}
// -----
#s = #ifrt.sharding_param<1x2 to [0] on 2>
!I = !ifrt.array<tensor<?x3xf32>, #s, [0, 1]>
!R = !reussir.rc<!reussir.array<? x 3 x f32, #reussir.target<devices = [0, 1], sharding = #s>>>
module @kernel attributes {sym_visibility = "private"} {
  func.func @main() -> tensor<?x3xf32> {
    %out = stablehlo.custom_call @test() : () -> tensor<?x3xf32>
    return %out : tensor<?x3xf32>
  }
}
func.func @uneven_static_axis(%token: !reussir.token<align : 8, size : 40>) -> !R attributes {ifrt.function} {
  %out, %done = ifrt.Call @kernel::@main() on devices [0, 1] : () -> !I
  // CHECK: array extent 3 is not divisible by 2 shards
  %result = reussir.array.from_ifrt %out token(%token : !reussir.token<align : 8, size : 40>) : !I -> !R
  return %result : !R
}

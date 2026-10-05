// REQUIRES: ifrt
// RUN: %not %reussir-opt %s --split-input-file --reussir-convert-to-llvm --mlir-print-op-on-diagnostic=false 2>&1 | %FileCheck %s

#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0], sharding = #s>>>
ifrt.LoadedExecutable @kernel on devices [0] : () -> !I
func.func @missing_token() -> !R attributes {ifrt.function} {
  %out, %done = ifrt.CallLoadedExecutable @kernel() : () -> !I
  // CHECK: 'reussir.array.from_ifrt' op requires an allocation token; run token instantiation first
  %result = reussir.array.from_ifrt %out : !I -> !R
  return %result : !R
}
// -----
!I = !ifrt.array<tensor<?xf32>, #ifrt.sharding_unspecified, [0, 1]>
!R = !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0, 1]>>>
ifrt.LoadedExecutable @kernel on devices [0, 1] : () -> !I
func.func @unresolved_sharding(%token: !reussir.token<align : 8, size : 40>) -> !R attributes {ifrt.function} {
  %out, %done = ifrt.CallLoadedExecutable @kernel() : () -> !I
  // CHECK: multi-device allocation requires concrete sharding
  %result = reussir.array.from_ifrt %out token(%token : !reussir.token<align : 8, size : 40>) : !I -> !R
  return %result : !R
}
// -----
#s = #ifrt.sharding_param<1x2 to [0] on 2>
!I = !ifrt.array<tensor<?x3xf32>, #s, [0, 1]>
!R = !reussir.rc<!reussir.array<? x 3 x f32, #reussir.target<devices = [0, 1], sharding = #s>>>
ifrt.LoadedExecutable @kernel on devices [0, 1] : () -> !I
func.func @uneven_static_axis(%token: !reussir.token<align : 8, size : 40>) -> !R attributes {ifrt.function} {
  %out, %done = ifrt.CallLoadedExecutable @kernel() : () -> !I
  // CHECK: array extent 3 is not divisible by 2 shards
  %result = reussir.array.from_ifrt %out token(%token : !reussir.token<align : 8, size : 40>) : !I -> !R
  return %result : !R
}

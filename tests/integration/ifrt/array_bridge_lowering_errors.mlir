// REQUIRES: ifrt
// RUN: %not %reussir-opt %s --split-input-file --reussir-convert-to-llvm --mlir-print-op-on-diagnostic=false 2>&1 | %FileCheck %s

#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<?xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0], sharding = #s>>>
func.func @dynamic_input(%input: !R) {
  // CHECK: 'reussir.array.to_ifrt' op dynamic-shape IFRT bridge lowering is not yet supported
  %view = reussir.array.to_ifrt %input : !R -> !I
  return
}
// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<?xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0], sharding = #s>>>
ifrt.LoadedExecutable @kernel on devices [0] : () -> !I
func.func @dynamic_output() -> !R attributes {ifrt.function} {
  %out, %done = ifrt.CallLoadedExecutable @kernel() : () -> !I
  // CHECK: 'reussir.array.from_ifrt' op dynamic-shape IFRT bridge lowering is not yet supported
  %result = reussir.array.from_ifrt %out : !I -> !R
  return %result : !R
}
// -----
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

// REQUIRES: ifrt
// RUN: %not %reussir-opt %s --split-input-file --reussir-token-instantiation --reussir-convert-to-llvm 2>&1 | %FileCheck %s

// CHECK: multi-device allocation requires concrete sharding
func.func @unspecified(%source: memref<4xf32>) {
  %d = reussir.array.to_device %source : memref<4xf32> -> !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0, 1]>>>
  return
}
// -----
// CHECK: array extent 3 is not divisible by 2 shards
func.func @uneven(%source: memref<3xf32>) {
  %d = reussir.array.to_device %source : memref<3xf32> -> !reussir.rc<!reussir.array<3 x f32, #reussir.target<devices = [0, 1], sharding = #ifrt.sharding_param<2 to [0] on 2>>>>
  return
}
// -----
// CHECK: host transfers do not support unreduced sharding axes
func.func @unreduced(%source: memref<4xf32>) {
  %d = reussir.array.to_device %source : memref<4xf32> -> !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0, 1], sharding = #ifrt.sharding_param<1 to [0] on 2 unreduced [0]>>>>
  return
}

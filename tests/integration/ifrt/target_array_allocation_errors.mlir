// REQUIRES: ifrt
// RUN: %not %reussir-opt %s --split-input-file --reussir-convert-to-llvm 2>&1 | %FileCheck %s

// CHECK: multi-device allocation requires concrete sharding
func.func @unspecified() {
  %a = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0, 1]>>>
  return
}
// -----
// CHECK: array extent 3 is not divisible by 2 shards
func.func @uneven() {
  %a = reussir.array.create extents() : !reussir.rc<!reussir.array<3 x 4 x f32, #reussir.target<devices = [0, 1], sharding = #ifrt.sharding_param<2x1 to [0] on 2>>>>
  return
}
// -----
// CHECK: Requires the same amount of `devices` and from `sharding`
func.func @wrong_device_count() {
  %a = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], sharding = #ifrt.sharding_param<2 to [0] on 2>>>>
  return
}
// -----
// CHECK: invalid allocation layout:
func.func @bad_layout() {
  %a = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x 4 x f32, #reussir.target<devices = [0], layout = "{0,0}">>>
  return
}
// -----
// CHECK: PjRt allocation layout supports dimension order and tiles only
func.func @unrepresentable_layout() {
  %a = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], layout = "{0:S(1)}">>>
  return
}
// -----
// CHECK: Device list has duplicate logical id 0
func.func @duplicate_devices() {
  %a = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0, 0], sharding = #ifrt.sharding_param<2 to [0] on 2>>>>
  return
}

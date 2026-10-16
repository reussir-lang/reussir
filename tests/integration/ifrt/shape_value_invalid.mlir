// RUN: %reussir-opt %s --split-input-file --verify-diagnostics
func.func @ranked(%source: memref<1xi64>) {
  // expected-error @+1 {{shape_value requires a rank-zero i64 upload}}
  %d = reussir.array.to_device %source shape_value : memref<1xi64> -> !reussir.rc<!reussir.array<1 x i64, #reussir.target<devices = [0]>>>
  return
}
// -----
func.func @wrong_element(%source: memref<i32>) {
  // expected-error @+1 {{shape_value requires a rank-zero i64 upload}}
  %d = reussir.array.to_device %source shape_value : memref<i32> -> !reussir.rc<!reussir.array<i32, #reussir.target<devices = [0]>>>
  return
}
// -----
func.func @wrong_attribute(%source: memref<i64>) {
  // expected-error @+1 {{attribute 'shape_value' failed to satisfy constraint: unit attribute}}
  %d = reussir.array.to_device %source {shape_value = 1 : i32} : memref<i64> -> !reussir.rc<!reussir.array<i64, #reussir.target<devices = [0]>>>
  return
}

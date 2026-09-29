// RUN: %not %reussir-opt %s --reussir-convert-to-llvm 2>&1 | %FileCheck %s
// CHECK: token is required but not provided
func.func @missing(%src: memref<4xf32>) {
  %d = reussir.array.to_device %src : memref<4xf32> -> !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0]>>>
  return
}

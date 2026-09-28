// RUN: %not %reussir-opt %s --reussir-convert-to-llvm 2>&1 | %FileCheck %s

// CHECK: token is required but not provided
func.func @missing() -> !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0]>>> {
  %a = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0]>>>
  return %a : !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0]>>>
}

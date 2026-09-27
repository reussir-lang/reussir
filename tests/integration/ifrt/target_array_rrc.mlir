// REQUIRES: ifrt
// RUN: %rrc %s --emit mlir-llvm -O none -o - | %FileCheck %s

// Exercise the XLA layout helper through the statically linked Rust driver.
// The C++ tools have a separate CMake link and cannot catch missing Cargo
// dependencies for the target-array lowering.
#target = #reussir.target<devices = [0], layout = "{0,1:T(8,128)}">
!array = !reussir.rc<!reussir.array<? x 4 x f32, #target>>

// CHECK: llvm.mlir.global {{.*}}dense<[8, 128]>
// CHECK-LABEL: llvm.func @create
// CHECK: llvm.call @__reussir_pjrt_array_allocate
func.func @create(%n: index) -> !array {
  %a = reussir.array.create extents(%n) : !array
  return %a : !array
}

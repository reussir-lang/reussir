// RUN: %not %reussir-opt %s --split-input-file --reussir-token-instantiation --reussir-convert-to-llvm 2>&1 | %FileCheck %s

// Unresolved metadata must not be silently replaced by the default layout.
// CHECK: auto layout must be resolved before allocation
func.func @placement() {
  %a = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], layout = "auto">>>
  return
}
// -----
// CHECK: unsupported PjRt element type: 'i7'
func.func @element_type() {
  %a = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x i7, #reussir.target<devices = [0]>>>
  return
}
// -----
// Host array creation must be expanded before LLVM conversion.
// CHECK: failed to legalize operation 'reussir.array.create'
func.func @unexpanded_host_array() {
  %a = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x i32>>
  return
}
// -----
// Ordinary reference drops must also be expanded before LLVM conversion.
// CHECK: failed to legalize operation 'reussir.ref.drop'
func.func @unexpanded_host_drop(%ref: !reussir.ref<i32>) {
  reussir.ref.drop(%ref : !reussir.ref<i32>)
  return
}

// UNSUPPORTED: ifrt
// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0]>>>
func.func @borrow(%input: !R) {
  // expected-error @+1 {{requires an OpenXLA-enabled build}}
  %view = reussir.array.to_ifrt %input : !R -> tensor<8xf32>
  return
}

// -----

!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0]>>>
func.func @adopt(%input: tensor<8xf32>) {
  // expected-error @+1 {{requires an OpenXLA-enabled build}}
  %result = reussir.array.from_ifrt %input : tensor<8xf32> -> !R
  return
}

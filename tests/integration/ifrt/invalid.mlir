// REQUIRES: ifrt
// RUN: %reussir-opt %s --split-input-file --verify-diagnostics --ifrt-verify-sharding-specified --ifrt-verify-donation

// Layout validation must still call the real XLA layout parser.
// expected-error @+1 {{Invalid layout mode:}}
func.func private @bad_layout(!ifrt.array<tensor<8xf32>, #ifrt.sharding_param<1 to [0] on 1>, [0], layout = "{not_a_dimension}">)

// -----

!array = !ifrt.array<tensor<8xf32>, #ifrt.sharding_unspecified, [0]>
// expected-error @+1 {{argument 0 has unspecified sharding}}
func.func @missing_sharding(%arg: !array) -> !array attributes {ifrt.function} {
  return %arg : !array
}

// -----

!array = !ifrt.array<tensor<8xf32>, #ifrt.sharding_param<1 to [0] on 1>, [0]>
func.func @double_donation(%arg: !array {ifrt.donated}) -> (!array, !array) attributes {ifrt.function} {
  %first, %ctrl0 = ifrt.CopyArrays(%arg) {donated = true} : (!array) -> !array
  // expected-error @+1 {{'ifrt.CopyArrays' op input #0 of op}}
  %second, %ctrl1 = ifrt.CopyArrays(%arg) {donated = true} : (!array) -> !array
  return %first, %second : !array, !array
}

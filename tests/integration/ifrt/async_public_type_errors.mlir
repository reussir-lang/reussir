// REQUIRES: ifrt
// RUN: %reussir-opt %s --split-input-file --reussir-ifrt-just-in-time-transform --verify-diagnostics
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
module {
  func.func @host(%input: !I) -> !I attributes {ifrt.function} {
    %out, %done = ifrt.Call @kernel::@main(%input) on devices [0] : (!I) -> !I
    return %out : !I
  }
  module @kernel {
    func.func @main(%arg: tensor<8xf32>) -> tensor<8xf32> { return %arg : tensor<8xf32> }
    // expected-error @+1 {{public kernel functions require ranked tensor signatures}}
    func.func @public_helper(%x: !stablehlo.token) -> !stablehlo.token { return %x : !stablehlo.token }
  }
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
module {
  func.func @host(%input: !I) -> !I attributes {ifrt.function} {
    %out, %done = ifrt.Call @kernel::@main(%input) on devices [0] : (!I) -> !I
    return %out : !I
  }
  module @kernel {
    func.func @main(%arg: tensor<8xf32>) -> tensor<8xf32> { return %arg : tensor<8xf32> }
    // expected-error @+1 {{public kernel functions require ranked tensor signatures}}
    func.func @public_helper(%x: !stablehlo.future<tensor<f32>>) -> !stablehlo.future<tensor<f32>> { return %x : !stablehlo.future<tensor<f32>> }
  }
}

// REQUIRES: ifrt
// RUN: %reussir-opt %s --split-input-file --reussir-ifrt-just-in-time-transform --verify-diagnostics

#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
module {
  func.func @host(%input: !I) -> !I attributes {ifrt.function} {
    // expected-error @+1 {{requires an outlined kernel module with @main; run IFRT outlining first}}
    %out, %done = ifrt.Call @kernel(%input) on devices [0] : (!I) -> !I
    return %out : !I
  }
  func.func private @kernel(%arg: tensor<8xf32>) -> tensor<8xf32> {
    return %arg : tensor<8xf32>
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
  // expected-error @+1 {{kernel requires a defined public @main function}}
  module @kernel {
    func.func nested @main(%arg: tensor<8xf32>) -> tensor<8xf32> {
      return %arg : tensor<8xf32>
    }
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
    // expected-error @+1 {{kernel cannot reference an external function}}
    func.func private @external(tensor<8xf32>) -> tensor<8xf32>
    func.func @main(%arg: tensor<8xf32>) -> tensor<8xf32> {
      %out = func.call @external(%arg) : (tensor<8xf32>) -> tensor<8xf32>
      return %out : tensor<8xf32>
    }
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
    func.func @main(%arg: tensor<8xf32>) -> tensor<8xf32> {
      // expected-error @+1 {{is not supported in a standalone StableHLO kernel}}
      %out = arith.addf %arg, %arg : tensor<8xf32>
      return %out : tensor<8xf32>
    }
  }
}

// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
module {
  func.func @host(%input: !I) -> !I attributes {ifrt.function} {
    // expected-error @+1 {{JIT preparation does not support external compile option overrides}}
    %out, %done = ifrt.Call @kernel::@main(%input) on devices [0] {ifrt.compile_options_key = "external"} : (!I) -> !I
    return %out : !I
  }
  module @kernel {
    func.func @main(%arg: tensor<8xf32>) -> tensor<8xf32> {
      return %arg : tensor<8xf32>
    }
  }
}

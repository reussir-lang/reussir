// REQUIRES: openxla
// RUN: %not %reussir-opt %s --split-input-file --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-convert-to-llvm --mlir-print-op-on-diagnostic=false 2>&1 | %FileCheck %s

#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<4xf32>, #s, [0]>
module {
  func.func @subset(%input: !I) -> !I attributes {ifrt.function} {
    // CHECK: runtime arrays must use all call devices in call order
    %out, %done = ifrt.Call @kernel::@main(%input) on devices [0, 1] : (!I) -> !I
    return %out : !I
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg: tensor<4xf32>) -> tensor<4xf32> { return %arg : tensor<4xf32> }
  }
}
// -----
#s = #ifrt.sharding_param<1 to [0] on 2>
!I = !ifrt.array<tensor<4xf32>, #s, [1, 0]>
module {
  func.func @order(%input: !I) -> !I attributes {ifrt.function} {
    // CHECK: runtime arrays must use all call devices in call order
    %out, %done = ifrt.Call @kernel::@main(%input) on devices [0, 1] : (!I) -> !I
    return %out : !I
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg: tensor<4xf32>) -> tensor<4xf32> { return %arg : tensor<4xf32> }
  }
}
// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<4xf32>, #s, [0]>
module {
  func.func @donation(%input: !I) -> !I attributes {ifrt.function} {
    // CHECK: runtime execution does not support donation or aliases
    %out, %done = ifrt.Call @kernel::@main(%input) on devices [0] {donated_input_indices = array<i32: 0>} : (!I) -> !I
    return %out : !I
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg: tensor<4xf32>) -> tensor<4xf32> { return %arg : tensor<4xf32> }
  }
}
// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<4xf32>, #s, [0]>
module {
  func.func @alias(%input: !I) -> !I attributes {ifrt.function} {
    // CHECK: runtime execution does not support donation or aliases
    %out, %done = ifrt.Call @kernel::@main(%input) on devices [0] {io_aliases = [array<i32: 0, 0>]} : (!I) -> !I
    return %out : !I
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg: tensor<4xf32>) -> tensor<4xf32> { return %arg : tensor<4xf32> }
  }
}
// -----
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<4xf32>, #s, [0]>
module {
  func.func @unadopted(%input: !I) -> !I attributes {ifrt.function} {
    // CHECK: runtime results must be adopted once with array.from_ifrt
    %out, %done = ifrt.Call @kernel::@main(%input) on devices [0] : (!I) -> !I
    return %out : !I
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg: tensor<4xf32>) -> tensor<4xf32> { return %arg : tensor<4xf32> }
  }
}

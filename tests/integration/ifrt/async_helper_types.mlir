// REQUIRES: openxla
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform | %FileCheck %s
// CHECK: reussir.pjrt.jit_call
// CHECK: reussir.ifrt.bytecode
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<8xf32>, #s, [0]>
module {
  func.func @host(%input: !I) -> !I attributes {ifrt.function} {
    %out, %done = ifrt.Call @kernel::@main(%input) on devices [0] : (!I) -> !I
    return %out : !I
  }
  module @kernel {
    func.func @main(%arg: tensor<8xf32>) -> tensor<8xf32> {
      %t = stablehlo.create_token : !stablehlo.token
      %ordered = func.call @token(%t) : (!stablehlo.token) -> !stablehlo.token
      return %arg : tensor<8xf32>
    }
    func.func private @token(%t: !stablehlo.token) -> !stablehlo.token {
      %joined = stablehlo.after_all %t, %t : !stablehlo.token
      return %joined : !stablehlo.token
    }
    func.func private @future(%f: !stablehlo.future<tensor<?x0xf32>, tensor<i32>>) -> !stablehlo.future<tensor<?x0xf32>, tensor<i32>> {
      return %f : !stablehlo.future<tensor<?x0xf32>, tensor<i32>>
    }
  }
}

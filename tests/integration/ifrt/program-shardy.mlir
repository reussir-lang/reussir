// REQUIRES: ifrt
// RUN: %reussir-opt %s --ifrt-to-outlined-atom-programs-pipeline -o %t.mlir
// RUN: %FileCheck %s < %t.mlir
// RUN: %reussir-ifrt-translate %t.mlir --serialize -o %t.ifrt.bc
// RUN: %reussir-ifrt-translate %t.ifrt.bc --deserialize | %FileCheck %s

!array = !ifrt.array<tensor<8xf32>, #ifrt.sharding_param<2 to [0] on 2>, [0, 1]>
module @model {
  sdy.mesh @mesh = <["x"=2]>
  // CHECK-LABEL: func.func @main
  // CHECK: ifrt.Call @increment::@main
  func.func @main(%arg: !array) -> !array attributes {ifrt.function} {
    %out, %done = ifrt.Call @increment(%arg) on devices [0, 1] : (!array) -> !array
    return %out : !array
  }

  // The mesh must move with the kernel and survive serialization inside its
  // versioned atom program, so the sharding symbol still resolves on load.
  // CHECK-LABEL: module @increment
  // CHECK: sdy.mesh @mesh = <["x"=2]>
  // CHECK: func.func @main
  // CHECK-SAME: sdy.sharding = #sdy.sharding<@mesh, [{"x"}]>
  // CHECK: stablehlo.add
  func.func private @increment(
      %arg: tensor<8xf32> {sdy.sharding = #sdy.sharding<@mesh, [{"x"}]>})
      -> (tensor<8xf32> {sdy.sharding = #sdy.sharding<@mesh, [{"x"}]>}) {
    %one = stablehlo.constant dense<1.0> : tensor<8xf32>
    %out = stablehlo.add %arg, %one : tensor<8xf32>
    return %out : tensor<8xf32>
  }
}

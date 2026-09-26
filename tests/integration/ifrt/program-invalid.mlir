// REQUIRES: ifrt
// RUN: %not %reussir-ifrt-translate %s --serialize 2>&1 | %FileCheck %s --check-prefix=UNOUTLINED
// RUN: %reussir-opt %s --ifrt-to-outlined-atom-programs-pipeline -o %t.mlir
// RUN: %not %reussir-ifrt-translate %t.mlir --serialize --ifrt_version=99.0.0 2>&1 | %FileCheck %s --check-prefix=VERSION
// RUN: %not %reussir-ifrt-translate %t.mlir --serialize --atom_program_version=99.0.0 2>&1 | %FileCheck %s --check-prefix=VERSION
// RUN: %reussir-opt %t.mlir --emit-bytecode -o %t.raw.bc
// RUN: %not %reussir-ifrt-translate %t.raw.bc --deserialize

// UNOUTLINED: failed to serialize
// UNOUTLINED-SAME: has not been outlined to a module
// VERSION: failed to serialize
!array = !ifrt.array<tensor<4xi32>, #ifrt.sharding_param<1 to [0] on 1>, [0]>
module @model {
  func.func @main(%arg: !array) -> !array attributes {ifrt.function} {
    %out, %done = ifrt.Call @increment(%arg) on devices [0] : (!array) -> !array
    return %out : !array
  }
  func.func private @increment(%arg: tensor<4xi32>) -> tensor<4xi32> {
    %one = stablehlo.constant dense<1> : tensor<4xi32>
    %out = stablehlo.add %arg, %one : tensor<4xi32>
    return %out : tensor<4xi32>
  }
}

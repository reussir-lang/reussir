// REQUIRES: stablehlo
// RUN: %reussir-opt %s --stablehlo-legalize-to-vhlo --emit-bytecode -o %t
// RUN: %reussir-opt %t --stablehlo-deserialize | %FileCheck %s

// CHECK-LABEL: func.func @add
// CHECK: stablehlo.add
func.func @add(%lhs: tensor<4xf32>, %rhs: tensor<4xf32>) -> tensor<4xf32> {
  %sum = stablehlo.add %lhs, %rhs : tensor<4xf32>
  return %sum : tensor<4xf32>
}

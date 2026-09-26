// REQUIRES: stablehlo
// RUN: %reussir-opt %s --chlo-legalize-to-stablehlo | %FileCheck %s

// CHECK-LABEL: func.func @broadcast_add
// CHECK: stablehlo.add
func.func @broadcast_add(%lhs: tensor<4xf32>, %rhs: tensor<4xf32>) -> tensor<4xf32> {
  %sum = chlo.broadcast_add %lhs, %rhs : (tensor<4xf32>, tensor<4xf32>) -> tensor<4xf32>
  return %sum : tensor<4xf32>
}

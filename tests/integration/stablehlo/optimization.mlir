// REQUIRES: stablehlo
// RUN: %reussir-opt %s --stablehlo-target-independent-optimization | %FileCheck %s

// CHECK-LABEL: func.func @add_zero
// CHECK-SAME: %[[ARG:.*]]: tensor<4xi32>
// CHECK-NEXT: return %[[ARG]] : tensor<4xi32>
func.func @add_zero(%arg: tensor<4xi32>) -> tensor<4xi32> {
  %zero = stablehlo.constant dense<0> : tensor<4xi32>
  %sum = stablehlo.add %arg, %zero : tensor<4xi32>
  return %sum : tensor<4xi32>
}

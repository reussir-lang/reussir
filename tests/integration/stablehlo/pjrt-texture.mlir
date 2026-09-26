// REQUIRES: stablehlo
// RUN: %reussir-opt %S/../../../crates/reussir-pjrt-sys/tests/fixtures/texture.mlir --stablehlo-legalize-to-vhlo --emit-bytecode -o %t
// RUN: %reussir-opt %t --stablehlo-deserialize | %FileCheck %s
// CHECK-LABEL: func.func @main
// CHECK: stablehlo.iota dim = 0
// CHECK: stablehlo.iota dim = 1
// CHECK: stablehlo.add
// CHECK: stablehlo.and
// CHECK: stablehlo.convert
// CHECK: return {{.*}} : tensor<8x8xf32>

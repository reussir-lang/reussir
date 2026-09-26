// REQUIRES: ifrt
// RUN: %reussir-opt %s --ifrt-to-outlined-atom-programs-pipeline -o %t.mlir
// RUN: %FileCheck %s < %t.mlir
// RUN: %reussir-ifrt-translate %t.mlir --serialize --strip_debuginfo -o %t.ifrt.bc
// RUN: %reussir-ifrt-translate %t.ifrt.bc --deserialize -o %t.restored.mlir
// RUN: %reussir-opt %t.restored.mlir --ifrt-verify-donation --ifrt-verify-sharding-specified | %FileCheck %s
// RUN: %reussir-ifrt-translate %t.restored.mlir --serialize --ifrt_version=0.5.0 --atom_program_version=1.11.0 -o %t.old.bc
// RUN: %reussir-ifrt-translate %t.old.bc --deserialize | %FileCheck %s

// Exercise the runtime's Serialized/IfrtIrProgramProto envelope and its actual
// program deserializer, including multiple atom programs and a shared callee.
// Raw MLIR bytecode alone cannot round-trip through this deserializer.
!array = !ifrt.array<tensor<8xf32>, #ifrt.sharding_param<2 to [0] on 2>, [0, 1], memory_kind = "device", layout = "{0}">

// CHECK: #{{.*}} = #ifrt.sharding_param<2 to [0] on 2>
module @model {
  // CHECK-LABEL: func.func @main
  // CHECK-SAME: tensor<8xf32>
  // CHECK-SAME: memory_kind = "device", layout = "{0}"
  // CHECK: ifrt.Call @increment::@main
  // CHECK-SAME: on devices [0, 1]
  // CHECK: ifrt.Call @twice::@main
  // CHECK: ifrt.Call @increment::@main
  // CHECK: return
  func.func @main(%arg: !array) -> !array attributes {ifrt.function} {
    %a, %ca = ifrt.Call @increment(%arg) on devices [0, 1] : (!array) -> !array
    %b, %cb = ifrt.Call @twice(%a) on devices [0, 1] : (!array) -> !array
    %c, %cc = ifrt.Call @increment(%b) on devices [0, 1] : (!array) -> !array
    return %c : !array
  }

  // CHECK-LABEL: module @increment
  // CHECK: func.func @main
  // CHECK: stablehlo.constant dense<1.000000e+00>
  // CHECK: stablehlo.add
  func.func private @increment(%arg: tensor<8xf32>) -> tensor<8xf32> {
    %one = stablehlo.constant dense<1.0> : tensor<8xf32>
    %out = stablehlo.add %arg, %one : tensor<8xf32>
    return %out : tensor<8xf32>
  }

  // CHECK-LABEL: module @twice
  // CHECK: func.func @main
  // CHECK: stablehlo.add
  // CHECK-NOT: module @
  func.func private @twice(%arg: tensor<8xf32>) -> tensor<8xf32> {
    %out = stablehlo.add %arg, %arg : tensor<8xf32>
    return %out : tensor<8xf32>
  }
}

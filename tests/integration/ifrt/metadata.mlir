// REQUIRES: ifrt
// RUN: %reussir-opt %s --ifrt-verify-sharding-specified --emit-bytecode -o %t
// RUN: %reussir-opt %t | %FileCheck %s

// Keep memory placement, device IDs, sharding, and explicit XLA layout through
// the actual registered dialect's parser, verifier, and MLIR bytecode path.
!host = !ifrt.array<tensor<8xf32>, #ifrt.sharding_param<2 to [0] on 2>, [0, 1], memory_kind = "pinned_host", layout = "{0}">
!device = !ifrt.array<tensor<8xf32>, #ifrt.sharding_param<2 to [0] on 2>, [0, 1], memory_kind = "device", layout = "{0}">

// CHECK: #[[SHARD:.*]] = #ifrt.sharding_param<2 to [0] on 2>
// CHECK-LABEL: func.func @copy_to_device
// CHECK-SAME: memory_kind = "pinned_host", layout = "{0}"
// CHECK-SAME: memory_kind = "device", layout = "{0}"
// CHECK: ifrt.CopyArrays
// CHECK-SAME: #[[SHARD]], [0, 1]
func.func @copy_to_device(%arg: !host) -> !device attributes {ifrt.function} {
  %copy, %control = ifrt.CopyArrays(%arg) : (!host) -> !device
  return %copy : !device
}

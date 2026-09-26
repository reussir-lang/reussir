// REQUIRES: ifrt
// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s
// RUN: %reussir-opt %s --emit-bytecode -o %t
// RUN: %reussir-opt %t | %FileCheck %s

// IFRT's sharding remains a typed nested attribute, including mesh permutation.
// CHECK: #[[SHARD:.*]] = #ifrt.sharding_param<2x1 to [1, 0] on 2x2>
// CHECK: reussir.full = #reussir.target<devices = [3, 1, 2, 0], sharding = #[[SHARD]], memory_kind = "device", layout = "{1,0}">
// CHECK: reussir.sharding_only = #reussir.target<devices = [3, 1, 2, 0], sharding = #[[SHARD]]>
// CHECK: reussir.unspecified = #reussir.target<devices = [0], sharding = #ifrt.sharding_unspecified>
module attributes {
  reussir.full = #reussir.target<devices = [3, 1, 2, 0], sharding = #ifrt.sharding_param<2x1 to [1, 0] on 2x2>, memory_kind = "device", layout = "{1,0}">,
  reussir.sharding_only = #reussir.target<devices = [3, 1, 2, 0], sharding = #ifrt.sharding_param<2x1 to [1, 0] on 2x2>>,
  reussir.unspecified = #reussir.target<devices = [0], sharding = #ifrt.sharding_unspecified>
} {}

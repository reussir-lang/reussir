// REQUIRES: ifrt
// RUN: %reussir-opt %s --ifrt-legalize-to-vifrt --vifrt-to-version='target_version=0.5.0' --emit-bytecode -o %t
// RUN: %reussir-opt %t | %FileCheck %s --check-prefix=VERSIONED
// RUN: %reussir-opt %t --vifrt-legalize-to-ifrt --ifrt-verify-donation | %FileCheck %s

!array = !ifrt.array<tensor<8xf32>, #ifrt.sharding_param<1 to [0] on 1>, [0], memory_kind = "device", layout = "{0}">

// VERSIONED: vifrt.FuncV1
// VERSIONED: vifrt.CopyArraysV2
// CHECK-LABEL: func.func @copy
// CHECK-SAME: memory_kind = "device", layout = "{0}"
// CHECK: ifrt.CopyArrays
// CHECK: return
func.func @copy(%arg: !array) -> !array attributes {ifrt.function} {
  %result, %control = ifrt.CopyArrays(%arg) : (!array) -> !array
  return %result : !array
}

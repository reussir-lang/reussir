// REQUIRES: ifrt
// RUN: %reussir-opt %s --ifrt-insert-copy-arrays-for-returned-many-times --ifrt-verify-donation | %FileCheck %s

!array = !ifrt.array<tensor<8xf32>, #ifrt.sharding_param<1 to [0] on 1>, [0], memory_kind = "device">

// Returning a donated argument twice must produce distinct arrays and transfer
// ownership only on the last copy.
// CHECK-LABEL: func.func @duplicate
// CHECK-SAME: %[[ARG:[^:]+]]:
// CHECK: %[[FIRST:[^,]+]], %{{.*}} = ifrt.CopyArrays(%[[ARG]]) :
// CHECK: %[[LAST:[^,]+]], %{{.*}} = ifrt.CopyArrays(%[[ARG]]) {donated = true}
// CHECK: return %[[FIRST]], %[[LAST]]
func.func @duplicate(%arg: !array {ifrt.donated}) -> (!array, !array) attributes {ifrt.function} {
  return %arg, %arg : !array, !array
}

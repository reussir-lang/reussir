// REQUIRES: ifrt
// RUN: %reussir-opt %s --emit-bytecode -o %t
// RUN: %reussir-opt %t | %FileCheck %s
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --reussir-token-reuse --ifrt-verify-donation | %FileCheck %s --check-prefix=TOKEN

#shard = #ifrt.sharding_param<1x1 to [0] on 1>
#target = #reussir.target<devices = [0], sharding = #shard, memory_kind = "device", layout = "{1,0}">
!I = !ifrt.array<tensor<?x4xf32>, #shard, [0], memory_kind = "device", layout = "{1,0}">
!R = !reussir.rc<!reussir.array<? x 4 x f32, #target>>

// The input stays owned by the caller. The result gets its own RC metadata.
// CHECK-LABEL: func.func @bridge
// CHECK: reussir.array.to_ifrt
// CHECK: ifrt.Call @kernel
// CHECK: reussir.array.from_ifrt
// TOKEN-LABEL: func.func @bridge
// TOKEN: reussir.token.alloc : <align : 8, size : 32>
// TOKEN: reussir.array.from_ifrt {{.*}} token(
func.func @bridge(%input: !R) -> !R attributes {ifrt.function} {
  %view = reussir.array.to_ifrt %input : !R -> !I
  %out, %done = ifrt.Call @kernel(%view) on devices [0] : (!I) -> !I
  %result = reussir.array.from_ifrt %out : !I -> !R
  return %result : !R
}
func.func private @kernel(%input: tensor<?x4xf32>) -> tensor<?x4xf32> {
  return %input : tensor<?x4xf32>
}

// An unrelated old array can supply the result's host metadata token.
// TOKEN-LABEL: func.func @reuse
// TOKEN: %[[OLD:.*]] = reussir.rc.dec
// TOKEN: %[[TOKEN:.*]] = reussir.token.ensure(%[[OLD]]
// TOKEN-NOT: reussir.token.alloc
// TOKEN: reussir.array.from_ifrt {{.*}} token(%[[TOKEN]]
func.func @reuse(%old: !R, %input: !R) -> !R attributes {ifrt.function} {
  %view = reussir.array.to_ifrt %input : !R -> !I
  %out, %done = ifrt.Call @kernel(%view) on devices [0] : (!I) -> !I
  reussir.rc.dec(%old : !R)
  %result = reussir.array.from_ifrt %out : !I -> !R
  return %result : !R
}

// Ordered multi-device placement and static shape survive the bridge.
#multi = #ifrt.sharding_param<2 to [0] on 2>
!MI = !ifrt.array<tensor<8xf32>, #multi, [3, 1]>
!MR = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [3, 1], sharding = #multi>> atomic>
ifrt.LoadedExecutable @loaded on devices [3, 1] : (!MI) -> !MI
// CHECK-LABEL: func.func @multi
// CHECK: reussir.array.to_ifrt
// CHECK: ifrt.After
// CHECK: ifrt.CallLoadedExecutable
// CHECK: reussir.array.from_ifrt
// TOKEN-LABEL: func.func @multi
// TOKEN: reussir.token.alloc : <align : 8, size : 32>
func.func @multi(%input: !MR) -> !MR attributes {ifrt.function} {
  %view = reussir.array.to_ifrt %input : !MR -> !MI
  %ready = "ifrt.After"(%view) : (!MI) -> !ifrt.control
  %out, %done = ifrt.CallLoadedExecutable @loaded(%view) after %ready : (!MI) -> !MI
  %result = reussir.array.from_ifrt %out : !MI -> !MR
  return %result : !MR
}

// Aliasing an IFRT-owned, donated argument transfers its ownership to the result.
// CHECK-LABEL: func.func @owned_alias
// CHECK: io_aliases
// CHECK: reussir.array.from_ifrt
func.func @owned_alias(%input: !MI {ifrt.donated}) -> !MR attributes {ifrt.function} {
  %out, %done = ifrt.CallLoadedExecutable @loaded(%input) {io_aliases = [array<i32: 0, 0>]} : (!MI) -> !MI
  %result = reussir.array.from_ifrt %out : !MI -> !MR
  return %result : !MR
}

// Unspecified metadata stays unspecified, including absent memory and layout.
!UI = !ifrt.array<tensor<8xf32>, #ifrt.sharding_unspecified, [0]>
!UR = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [0]>>>
// CHECK-LABEL: func.func @unspecified
// CHECK: reussir.array.to_ifrt
func.func @unspecified(%input: !UR) {
  %view = reussir.array.to_ifrt %input : !UR -> !UI
  return
}

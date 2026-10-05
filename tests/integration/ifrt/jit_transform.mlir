// REQUIRES: ifrt
// RUN: %reussir-opt %s --ifrt-to-outlined-atom-programs-pipeline --reussir-ifrt-just-in-time-transform --symbol-dce -o %t.mlir
// RUN: %FileCheck %s --implicit-check-not=ifrt.function < %t.mlir
// RUN: %reussir-opt %S/program-shardy.mlir --ifrt-to-outlined-atom-programs-pipeline --reussir-ifrt-just-in-time-transform --symbol-dce | %FileCheck %s --check-prefix=SHARDY
// RUN: %reussir-opt %t.mlir --reussir-ifrt-just-in-time-transform --emit-bytecode -o %t.bc
// RUN: %reussir-opt %t.bc | %FileCheck %s --implicit-check-not=ifrt.function
// RUN: %reussir-opt %t.mlir --reussir-attach-native-target --reussir-token-instantiation --reussir-token-reuse | %FileCheck %s --check-prefix=TOKEN

#s = #ifrt.sharding_param<1 to [0] on 1>
#target = #reussir.target<devices = [0], sharding = #s, memory_kind = "device", layout = "{0}">
!I = !ifrt.array<tensor<8xf32>, #s, [0], memory_kind = "device", layout = "{0}">
!R = !reussir.rc<!reussir.array<8 x f32, #target>>

module {
  // CHECK-LABEL: func.func @main
  // CHECK: %[[VIEW:.*]] = reussir.array.to_ifrt
  // CHECK: %[[OUT:.*]], %[[DONE:.*]] = reussir.pjrt.jit_call @[[CODE:[A-Za-z0-9_]+]](%[[VIEW]]) on devices [0]
  // CHECK: reussir.rc.dec
  // CHECK: reussir.array.from_ifrt %[[OUT]]
  // CHECK: reussir.array.to_host
  // CHECK: return
  // TOKEN-LABEL: func.func @main
  // TOKEN: %[[REUSE:.*]] = reussir.rc.dec
  // TOKEN: %[[TOKEN:.*]] = reussir.token.ensure(%[[REUSE]]
  // TOKEN: reussir.array.from_ifrt {{.*}} token(%[[TOKEN]]
  func.func @main(%src: memref<8xf32>, %dst: memref<8xf32>, %old: !R) attributes {ifrt.function} {
    %input = reussir.array.to_device %src : memref<8xf32> -> !R
    %view = reussir.array.to_ifrt %input : !R -> !I
    %out, %done = ifrt.Call @double(%view) on devices [0] : (!I) -> !I
    reussir.rc.dec(%old : !R)
    %result = reussir.array.from_ifrt %out : !I -> !R
    reussir.array.to_host %result into %dst : !R, memref<8xf32>
    reussir.rc.dec(%input : !R)
    reussir.rc.dec(%result : !R)
    return
  }
  // CHECK-NOT: module @double
  // CHECK-NOT: stablehlo.add
  // CHECK: reussir.ifrt.bytecode "private" @[[CODE]] bytecode "ML{{.*}}" checksum "{{([0-9a-f]{64})}}"
  // CHECK-NOT: reussir.ifrt.bytecode
  // CHECK-NOT: module @double
  func.func private @double(%arg: tensor<8xf32>) -> tensor<8xf32> {
    %out = stablehlo.add %arg, %arg : tensor<8xf32>
    return %out : tensor<8xf32>
  }
}

// Shardy meshes are resolved inside the serialized module during verification.
// SHARDY: reussir.pjrt.jit_call
// SHARDY-SAME: on devices [0, 1]
// SHARDY-NOT: module @increment
// SHARDY: reussir.ifrt.bytecode

// REQUIRES: ifrt
// RUN: %rrc %S/donation_e2e.mlir --emit llvm-ir -O aggressive -o %t.ll
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-pjrt-donation | %FileCheck %s
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<4xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], sharding = #s>>>
module {
  // CHECK-LABEL: func.func @while_loop
  // CHECK: reussir.rc.is_unique
  // CHECK: scf.while
  // CHECK-NOT: reussir.rc.is_unique
  // CHECK: donation_variants
  func.func @while_loop(%a: !R, %n: index) -> !R attributes {ifrt.function} {
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    %result:2 = scf.while (%state = %a, %i = %zero) : (!R, index) -> (!R, index) {
      %continue = arith.cmpi slt, %i, %n : index
      scf.condition(%continue) %state, %i : !R, index
    } do {
    ^bb0(%state: !R, %i: index):
      %v = reussir.array.to_ifrt %state : !R -> !I
      %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
      %next = reussir.array.from_ifrt %o : !I -> !R
      reussir.rc.dec(%state : !R)
      %j = arith.addi %i, %one : index
      scf.yield %next, %j : !R, index
    }
    return %result#0 : !R
  }

  // CHECK-LABEL: func.func @forwarded_live_alias
  // CHECK-NOT: donation_variants
  // CHECK: return
  func.func @forwarded_live_alias(%a: !R, %condition: i1, %dst: memref<4xf32>) -> !R attributes {ifrt.function} {
    %forwarded = scf.if %condition -> !R {
      scf.yield %a : !R
    } else {
      scf.yield %a : !R
    }
    %v = reussir.array.to_ifrt %forwarded : !R -> !I
    %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %result = reussir.array.from_ifrt %o : !I -> !R
    reussir.array.to_host %a into %dst : !R, memref<4xf32>
    reussir.rc.dec(%forwarded : !R)
    return %result : !R
  }
  // CHECK-LABEL: func.func @forwarded_dead_alias
  // CHECK: donation_variants
  func.func @forwarded_dead_alias(%a: !R, %condition: i1) -> !R attributes {ifrt.function} {
    %forwarded = scf.if %condition -> !R {
      scf.yield %a : !R
    } else {
      scf.yield %a : !R
    }
    %v = reussir.array.to_ifrt %forwarded : !R -> !I
    %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %result = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%forwarded : !R)
    return %result : !R
  }
  func.func private @observe(!R)
  // CHECK-LABEL: func.func @opaque_observer
  // CHECK: call @observe
  // CHECK: reussir.rc.is_unique
  // CHECK: donation_variants
  func.func @opaque_observer(%host: memref<4xf32>) -> !R attributes {ifrt.function} {
    %a = reussir.array.to_device %host : memref<4xf32> -> !R
    func.call @observe(%a) : (!R) -> ()
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %result = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    return %result : !R
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%x: tensor<4xf32>) -> tensor<4xf32> {
      %one = stablehlo.constant dense<1.0> : tensor<4xf32>
      %r = stablehlo.add %x, %one : tensor<4xf32>
      return %r : tensor<4xf32>
    }
  }
  module @pair attributes {sym_visibility = "private"} {
    func.func @main(%x: tensor<4xf32>, %y: tensor<4xf32>) -> tensor<4xf32> {
      %r = stablehlo.add %x, %y : tensor<4xf32>
      return %r : tensor<4xf32>
    }
  }
}

// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-pjrt-donation | %FileCheck %s
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<4xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], sharding = #s>>>
module {
  func.func private @fresh_result(%host: memref<4xf32>) -> !R {
    %a = reussir.array.to_device %host : memref<4xf32> -> !R
    return %a : !R
  }
  func.func private @identity(%a: !R) -> !R { return %a : !R }
  // CHECK-LABEL: func.func @cfg_join
  // CHECK: reussir.rc.is_unique
  // CHECK: donation_mapping = array<i32: 0>
  func.func @cfg_join(%host: memref<4xf32>, %input: !R, %condition: i1) -> !R attributes {ifrt.function} {
    %f = reussir.array.to_device %host : memref<4xf32> -> !R
    cf.cond_br %condition, ^join(%f : !R), ^join(%input : !R)
  ^join(%a: !R):
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    return %r : !R
  }
  // CHECK-LABEL: func.func @dead_predecessor
  // CHECK-NOT: reussir.rc.is_unique
  // CHECK: donation_mapping = array<i32: 0>
  func.func @dead_predecessor(%host: memref<4xf32>, %input: !R) -> !R attributes {ifrt.function} {
    %yes = arith.constant true
    %a = scf.if %yes -> !R {
      %f = reussir.array.to_device %host : memref<4xf32> -> !R
      scf.yield %f : !R
    } else {
      scf.yield %input : !R
    }
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    return %r : !R
  }
  // CHECK-LABEL: func.func @retained_forwarded
  // CHECK: reussir.rc.is_unique
  // CHECK: donation_mapping = array<i32: 0>
  func.func @retained_forwarded(%host: memref<4xf32>, %condition: i1) -> !R attributes {ifrt.function} {
    %original = reussir.array.to_device %host : memref<4xf32> -> !R
    reussir.rc.inc(%original : !R)
    %a = scf.if %condition -> !R {
      scf.yield %original : !R
    } else {
      scf.yield %original : !R
    }
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    reussir.rc.dec(%original : !R)
    return %r : !R
  }
  // CHECK-LABEL: func.func @loop_backedge
  // CHECK: scf.for
  // CHECK: reussir.rc.is_unique
  // CHECK: donation_mapping = array<i32: 0>
  func.func @loop_backedge(%host: memref<4xf32>, %fallback: !R, %n: index) -> !R attributes {ifrt.function} {
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    %fresh = reussir.array.to_device %host : memref<4xf32> -> !R
    %result = scf.for %i = %zero to %n step %one iter_args(%a = %fresh) -> !R {
      %v = reussir.array.to_ifrt %a : !R -> !I
      %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
      %r = reussir.array.from_ifrt %o : !I -> !R
      reussir.rc.dec(%a : !R)
      reussir.rc.dec(%r : !R)
      reussir.rc.inc(%fallback : !R)
      scf.yield %fallback : !R
    }
    return %result : !R
  }
  // CHECK-LABEL: func.func @cfg_live_alias
  // CHECK-NOT: may_donate
  // CHECK: return
  func.func @cfg_live_alias(%input: !R, %dst: memref<4xf32>) -> !R attributes {ifrt.function} {
    cf.br ^next(%input : !R)
  ^next(%a: !R):
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.array.to_host %input into %dst : !R, memref<4xf32>
    reussir.rc.dec(%a : !R)
    return %r : !R
  }
  // CHECK-LABEL: func.func @callee_fresh
  // CHECK-NOT: reussir.rc.is_unique
  // CHECK: donation_mapping = array<i32: 0>
  func.func @callee_fresh(%host: memref<4xf32>) -> !R attributes {ifrt.function} {
    %a = func.call @fresh_result(%host) : (memref<4xf32>) -> !R
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    return %r : !R
  }
  // CHECK-LABEL: func.func @callee_argument
  // CHECK: reussir.rc.is_unique
  // CHECK: donation_mapping = array<i32: 0>
  func.func @callee_argument(%input: !R) -> !R attributes {ifrt.function} {
    %a = func.call @identity(%input) : (!R) -> !R
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    return %r : !R
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%x: tensor<4xf32>) -> tensor<4xf32> {
      %one = stablehlo.constant dense<1.0> : tensor<4xf32>
      %r = stablehlo.add %x, %one : tensor<4xf32>
      return %r : tensor<4xf32>
    }
  }
}

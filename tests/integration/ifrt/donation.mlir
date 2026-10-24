// REQUIRES: openxla
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-pjrt-donation | %FileCheck %s
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-pjrt-donation --reussir-pjrt-donation | %FileCheck %s
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-pjrt-donation='max-analysis-work=1' | %FileCheck %s --check-prefix=NONE
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-pjrt-donation='infer-donation=false' | %FileCheck %s --check-prefix=NONE --implicit-check-not=may_donate --implicit-check-not=donation_guards
// NONE-NOT: may_donate

#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<4xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], sharding = #s>>>
module {
  // CHECK-LABEL: func.func @fresh
  // CHECK-NOT: reussir.rc.is_unique
  // CHECK: reussir.array.to_ifrt {{.*}} {may_donate}
  // CHECK: donation_mapping = array<i32: 0>
  func.func @fresh(%host: memref<4xf32>) -> !R attributes {ifrt.function} {
    %a = reussir.array.to_device %host : memref<4xf32> -> !R
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    return %r : !R
  }
  // CHECK-LABEL: func.func @guarded
  // CHECK: reussir.rc.is_unique
  // CHECK: donation_mapping = array<i32: 0>
  func.func @guarded(%a: !R) -> !R attributes {ifrt.function} {
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    return %r : !R
  }
  // CHECK-LABEL: func.func @live_contents
  // CHECK-NOT: may_donate
  // CHECK: return
  func.func @live_contents(%a: !R, %dst: memref<4xf32>) -> !R attributes {ifrt.function} {
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.array.to_host %a into %dst : !R, memref<4xf32>
    reussir.rc.dec(%a : !R)
    return %r : !R
  }
  // CHECK-LABEL: func.func @borrow_only
  // CHECK-NOT: may_donate
  // CHECK: return
  func.func @borrow_only(%a: !R) -> !R attributes {ifrt.function} {
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    return %r : !R
  }
  // CHECK-LABEL: func.func @temporary_share
  // CHECK: reussir.rc.is_unique
  // CHECK: reussir.rc.dec
  // CHECK-NOT: reussir.rc.is_unique
  // CHECK: donation_mapping = array<i32: 0>
  func.func @temporary_share(%host: memref<4xf32>) -> (!R, i1) attributes {ifrt.function} {
    %a = reussir.array.to_device %host : memref<4xf32> -> !R
    reussir.rc.inc(%a : !R)
    %shared = reussir.rc.is_unique(%a : !R) : i1
    reussir.rc.dec(%a : !R)
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    return %r, %shared : !R, i1
  }
  // CHECK-LABEL: func.func @branches
  // CHECK: scf.if
  // CHECK: donation_mapping = array<i32: 0>
  // CHECK: } else {
  // CHECK: donation_mapping = array<i32: 0>
  func.func @branches(%a: !R, %choose: i1) -> !R attributes {ifrt.function} {
    %r = scf.if %choose -> !R {
      %v = reussir.array.to_ifrt %a : !R -> !I
      %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
      %r = reussir.array.from_ifrt %o : !I -> !R
      reussir.rc.dec(%a : !R)
      scf.yield %r : !R
    } else {
      %v = reussir.array.to_ifrt %a : !R -> !I
      %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
      %r = reussir.array.from_ifrt %o : !I -> !R
      reussir.rc.dec(%a : !R)
      scf.yield %r : !R
    }
    return %r : !R
  }
  // CHECK-LABEL: func.func @loop_state
  // CHECK: scf.for
  // CHECK: donation_mapping = array<i32: 0>
  func.func @loop_state(%a: !R, %n: index) -> !R attributes {ifrt.function} {
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    %r = scf.for %i = %zero to %n step %one iter_args(%state = %a) -> !R {
      %v = reussir.array.to_ifrt %state : !R -> !I
      %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
      %next = reussir.array.from_ifrt %o : !I -> !R
      reussir.rc.dec(%state : !R)
      scf.yield %next : !R
    }
    return %r : !R
  }
  // CHECK-LABEL: func.func @captured
  // CHECK-NOT: may_donate
  // CHECK: return
  func.func @captured(%a: !R, %n: index) attributes {ifrt.function} {
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    scf.for %i = %zero to %n step %one {
      %v = reussir.array.to_ifrt %a : !R -> !I
      %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
      %r = reussir.array.from_ifrt %o : !I -> !R
      reussir.rc.dec(%r : !R)
    }
    reussir.rc.dec(%a : !R)
    return
  }
  // CHECK-LABEL: func.func @duplicate
  // CHECK: donation_mapping = array<i32: 0, 0>
  func.func @duplicate(%a: !R) -> !R attributes {ifrt.function} {
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %c = ifrt.Call @pair::@main(%v, %v) on devices [0] : (!I, !I) -> !I
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
  module @pair attributes {sym_visibility = "private"} {
    func.func @main(%x: tensor<4xf32>, %y: tensor<4xf32>) -> tensor<4xf32> {
      %r = stablehlo.add %x, %y : tensor<4xf32>
      return %r : tensor<4xf32>
    }
  }
}

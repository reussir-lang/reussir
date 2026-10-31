// REQUIRES: openxla
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-pjrt-donation | %FileCheck %s

// A borrowed array is never donated: its count can be 1 while the caller
// still holds it, so a runtime uniqueness guard would pass and the kernel
// would overwrite the caller's buffer. The hosts differ only in the
// parameter convention: the owned one releases its array after the call,
// the borrowed one leaves it to the caller.
#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<4xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], sharding = #s>>>
module {
  func.func @caller(%a: !R) -> (!R, !R) attributes {ifrt.function} {
    reussir.rc.inc(%a : !R)
    %x = func.call @owned(%a) : (!R) -> !R
    %y = func.call @borrowed(%a) : (!R) -> !R
    reussir.rc.dec(%a : !R)
    return %x, %y : !R, !R
  }
  // CHECK-LABEL: func.func private @owned
  // CHECK: reussir.rc.is_unique
  // CHECK: donation_mapping = array<i32: 0>
  func.func private @owned(%a: !R) -> !R attributes {ifrt.function} {
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    return %r : !R
  }
  // CHECK-LABEL: func.func private @borrowed
  // CHECK-NOT: reussir.rc.is_unique
  // CHECK-NOT: may_donate
  // CHECK: return
  func.func private @borrowed(%a: !R {reussir.borrowed = #reussir.borrowed}) -> !R attributes {ifrt.function} {
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %c = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    return %r : !R
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%arg0: tensor<4xf32>) -> tensor<4xf32> {
      return %arg0 : tensor<4xf32>
    }
  }
}

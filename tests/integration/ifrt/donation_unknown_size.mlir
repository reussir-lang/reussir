// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-pjrt-donation | %FileCheck %s
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-pjrt-donation='max-variants=2' | %FileCheck %s --check-prefix=KNOWN
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-pjrt-donation='max-candidates=1' | %FileCheck %s --check-prefix=KNOWN
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-pjrt-donation='max-extra-bytes=0' | %FileCheck %s --check-prefix=NONE
// NONE-NOT: donation_variants

#s = #ifrt.sharding_param<1 to [0] on 1>
!D = !ifrt.array<tensor<?xf32>, #s, [0]>
!S = !ifrt.array<tensor<4xf32>, #s, [0]>
!RD = !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0], sharding = #s>>>
!RS = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], sharding = #s>>>
module {
  // CHECK-LABEL: func.func @mixed
  // CHECK: reussir.rc.is_unique
  // CHECK: reussir.rc.is_unique
  // CHECK: donation_variants = [{{.*}}donors = array<i32: 1>{{.*}}donors = array<i32: 0>}]
  // KNOWN-LABEL: func.func @mixed
  // KNOWN: donation_variants = [{arguments = array<i32: 0, 1>, callee = @{{[^,]*}}, donors = array<i32: 1>}]
  func.func @mixed(%a: !RD, %b: !RS) -> (!RD, !RS) attributes {ifrt.function} {
    %av = reussir.array.to_ifrt %a : !RD -> !D
    %bv = reussir.array.to_ifrt %b : !RS -> !S
    %o:2, %c = ifrt.Call @kernel::@main(%av, %bv) on devices [0] : (!D, !S) -> (!D, !S)
    %r0 = reussir.array.from_ifrt %o#0 : !D -> !RD
    %r1 = reussir.array.from_ifrt %o#1 : !S -> !RS
    reussir.rc.dec(%a : !RD)
    reussir.rc.dec(%b : !RS)
    return %r0, %r1 : !RD, !RS
  }
  // CHECK-LABEL: func.func @live_dynamic
  // CHECK-NOT: donation_variants
  // CHECK: return
  func.func @live_dynamic(%a: !RD, %b: !RS, %dst: memref<?xf32>) -> (!RD, !RS) attributes {ifrt.function} {
    %av = reussir.array.to_ifrt %a : !RD -> !D
    %bv = reussir.array.to_ifrt %b : !RS -> !S
    %o:2, %c = ifrt.Call @kernel::@main(%av, %bv) on devices [0] : (!D, !S) -> (!D, !S)
    %r0 = reussir.array.from_ifrt %o#0 : !D -> !RD
    %r1 = reussir.array.from_ifrt %o#1 : !S -> !RS
    reussir.array.to_host %a into %dst : !RD, memref<?xf32>
    reussir.rc.dec(%a : !RD)
    return %r0, %r1 : !RD, !RS
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%a: tensor<?xf32>, %b: tensor<4xf32>) -> (tensor<?xf32>, tensor<4xf32>) {
      %r0 = stablehlo.add %a, %a : tensor<?xf32>
      %r1 = stablehlo.add %b, %b : tensor<4xf32>
      return %r0, %r1 : tensor<?xf32>, tensor<4xf32>
    }
  }
}

// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s
!test = !reussir.record<compound "Test" [regional] {i64, i64, [field] i64 }>
!inner = !reussir.record<variant "Inner" [value] { i64, i64 }>
module {
  // CHECK: func.func @acquire_basic(%arg0: !reussir.ref<i32>)
  func.func @acquire_basic(%ref : !reussir.ref<i32>) {
    // CHECK: reussir.ref.acquire(%arg0 : !reussir.ref<i32>)
    reussir.ref.acquire (%ref : !reussir.ref<i32>)
    return
  }

  func.func @acquire_rc(%ref : !reussir.ref<!reussir.rc<i64>>) {
    // CHECK: reussir.ref.acquire(%arg0 : !reussir.ref<!reussir.rc<i64>>)
    reussir.ref.acquire (%ref : !reussir.ref<!reussir.rc<i64>>)
    return
  }

  func.func @acquire_compound(%ref : !reussir.ref<!test>) {
    reussir.ref.acquire (%ref : !reussir.ref<!test>)
    return
  }

  func.func @acquire_inlined(%ref : !reussir.ref<!test>) {
    reussir.ref.acquire (%ref : !reussir.ref<!test>) inlined
    return
  }

  func.func @acquire_variant(%ref : !reussir.ref<!inner>) {
    reussir.ref.acquire (%ref : !reussir.ref<!inner>)
    return
  }

  func.func @acquire_variant_tagged(%ref : !reussir.ref<!inner>) {
    reussir.ref.acquire (%ref : !reussir.ref<!inner>) variant [0]
    return
  }
}

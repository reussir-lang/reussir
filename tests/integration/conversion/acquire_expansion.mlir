// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s

!inner = !reussir.record<compound "Inner" [shared] { i64 }>
!value_box = !reussir.record<compound "ValueBox" [value] {
  !inner, !inner, i32
}>

!list_ = !reussir.record<variant "List" incomplete>
!list_nil = !reussir.record<compound "List::Nil" [value] { }>
!list_cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
!list = !reussir.record<variant "List" { !list_nil, !list_cons }>

module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>> }  {
  // CHECK-LABEL: func.func @acquire_value_struct_with_rc
  // CHECK: reussir.ref.project{{.*}}[0]
  // CHECK: reussir.ref.load
  // CHECK: reussir.rc.inc
  // CHECK: reussir.ref.project{{.*}}[1]
  // CHECK: reussir.ref.load
  // CHECK: reussir.rc.inc
  // CHECK-NOT: reussir.ref.acquire
  func.func @acquire_value_struct_with_rc(%ref : !reussir.ref<!value_box>) {
    reussir.ref.acquire (%ref : !reussir.ref<!value_box>)
    return
  }

  // CHECK-LABEL: func.func @acquire_rc
  // CHECK: reussir.ref.load
  // CHECK: reussir.rc.inc
  // CHECK-NOT: reussir.ref.acquire
  func.func @acquire_rc(%ref : !reussir.ref<!reussir.rc<i64>>) {
    reussir.ref.acquire (%ref : !reussir.ref<!reussir.rc<i64>>)
    return
  }

  // CHECK-LABEL: func.func @acquire_trivial
  // CHECK-NOT: reussir.ref.acquire
  func.func @acquire_trivial(%ref : !reussir.ref<i32>) {
    reussir.ref.acquire (%ref : !reussir.ref<i32>)
    return
  }

  // Acquire on a variant record with a known tag: should coerce then acquire
  // CHECK-LABEL: func.func @acquire_variant_tagged
  // CHECK: reussir.record.coerce[1]
  // CHECK: reussir.ref.project
  // CHECK: reussir.ref.load
  // CHECK: reussir.rc.inc
  // CHECK-NOT: reussir.ref.acquire
  func.func @acquire_variant_tagged(%arg0: !reussir.ref<!list>) {
    reussir.ref.acquire (%arg0 : !reussir.ref<!list>) variant [1]
    return
  }

  // Acquire on a variant record without tag: should dispatch
  // CHECK-LABEL: func.func @acquire_variant
  // CHECK: reussir.record.dispatch
  // CHECK-NOT: reussir.ref.acquire
  func.func @acquire_variant(%arg0: !reussir.ref<!list>) {
    reussir.ref.acquire (%arg0 : !reussir.ref<!list>)
    return
  }
}

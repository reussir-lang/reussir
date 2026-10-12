// Repeated requests, including requests in newly outlined drop bodies, must
// resolve to the same glue symbols throughout one greedy rewrite.
// RUN: %reussir-opt %s '--reussir-acquire-drop-expansion=outline-record=true' | %FileCheck %s
// RUN: %reussir-opt %s '--reussir-acquire-drop-expansion=outline-record=true' '--reussir-acquire-drop-expansion=outline-record=true' | %FileCheck %s

!leaf = !reussir.record<compound "Leaf" [shared] {i64}>
!inner = !reussir.record<compound "Inner" [value] {!leaf}>
!outer = !reussir.record<compound "Outer" [value] {!inner, !inner}>

// CHECK-DAG: func.func private @_RINvNvC4core9intrinsic13drop_in_placeInnerE(
// CHECK-DAG: func.func private @_RINvNvC4core9intrinsic13drop_in_placeOuterE(
// CHECK-DAG: func.func private @_RINvNvC4core9intrinsic16acquire_in_placeOuterE(
// CHECK-LABEL: func.func @use
// CHECK: call @_RINvNvC4core9intrinsic16acquire_in_placeOuterE(
// CHECK: call @_RINvNvC4core9intrinsic16acquire_in_placeOuterE(
// CHECK: call @_RINvNvC4core9intrinsic13drop_in_placeOuterE(
// CHECK: call @_RINvNvC4core9intrinsic13drop_in_placeOuterE(
// CHECK: return
module {
  func.func @use(%arg: !reussir.ref<!outer>) {
    reussir.ref.acquire(%arg : !reussir.ref<!outer>)
    reussir.ref.acquire(%arg : !reussir.ref<!outer>)
    reussir.ref.drop(%arg : !reussir.ref<!outer>)
    reussir.ref.drop(%arg : !reussir.ref<!outer>)
    return
  }
}

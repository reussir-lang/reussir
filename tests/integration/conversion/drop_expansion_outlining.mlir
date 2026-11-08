// RUN: %reussir-opt %s --pass-pipeline='builtin.module(func.func(reussir-token-instantiation),reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-acquire-drop-expansion{expand-decrement=true outline-record=true},func.func(reussir-token-reuse))' | %FileCheck %s

// A decrement of a variant whose tag is unknown releases the payload through
// the outlined drop glue rather than a dispatch over every arm. When its token
// feeds an acceptor, the expanded decrement stays to donate it; otherwise the
// whole decrement becomes the outlined one.

!list = !reussir.record<variant "List" {!reussir.record<compound "List.Cons" [value] {i64, !reussir.record<variant "List">}>, !reussir.record<compound "List.Var" [value] {i64}>, !reussir.record<compound "List.Nil" [value] {}>}>
!rclist = !reussir.rc<!list>

module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>> } {
  // CHECK-LABEL: func.func @reused
  // CHECK:         %[[TOKEN:.*]] = scf.if
  // CHECK-NOT:       reussir.record.dispatch
  // CHECK:           func.call @{{.*}}drop_in_place{{.*}}(%{{.*}})
  // CHECK:         } {reussir.expanded_decrement}
  // CHECK:         reussir.token.realloc(%[[TOKEN]]
  func.func @reused(%0: !rclist, %x: i64) -> !rclist {
    reussir.rc.dec (%0 : !rclist)
    %r = "reussir.rc.create_variant"(%x) <{operandSegmentSizes = array<i32: 0, 1, 0, 0>, tag = 1 : index}> : (i64) -> !rclist
    return %r : !rclist
  }

  // CHECK-LABEL: func.func @unreused
  // CHECK-NEXT:    call @{{.*}}tail_release(%arg0)
  // CHECK-NEXT:    return
  func.func @unreused(%0: !rclist) {
    reussir.rc.dec (%0 : !rclist)
    return
  }
}

// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='drop-order=forward outline-record=true expand-decrement=true' --verify-each | %FileCheck %s --implicit-check-not=reussir.drop.defer --implicit-check-not=reussir.rc.defer
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=backward' --verify-each | %FileCheck %s --implicit-check-not=reussir.drop.defer --implicit-check-not=reussir.rc.defer
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='drop-order=forward outline-record=true expand-decrement=true tail-recursive-drops=false' --verify-each | %FileCheck %s --check-prefix=DISABLED --implicit-check-not=tail_release
!list_ = !reussir.record<compound "ArrayList" incomplete>
!list = !reussir.record<compound "ArrayList" { i64, !reussir.nullable<!reussir.rc<!list_>> }>
!wrapper = !reussir.record<compound "ListWrapper" [value] { !list }>
!outer = !reussir.record<compound "OuterWrapper" [value] { !wrapper }>
!boxed = !reussir.array<? x !reussir.rc<!list>>
!inlined = !reussir.array<? x !outer>
!atomic = !reussir.array<? x !reussir.nullable<!reussir.rc<!list shared atomic>>>
module {
  // CHECK-LABEL: func.func @boxed
  // CHECK: scf.for
  // CHECK: .tail_release
  // DISABLED-LABEL: func.func @boxed
  // DISABLED: reussir.drop.defer_indexed
  func.func @boxed(%ref: !reussir.ref<!boxed>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!boxed>)
    return
  }
  // CHECK-LABEL: func.func @inlined
  // CHECK: scf.for
  // CHECK: .tail_release
  func.func @inlined(%ref: !reussir.ref<!inlined>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!inlined>)
    return
  }
  // CHECK-LABEL: func.func @atomic
  // CHECK: scf.for
  // CHECK: .tail_release
  func.func @atomic(%ref: !reussir.ref<!atomic unspecified atomic>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!atomic unspecified atomic>)
    return
  }
  // CHECK-LABEL: func.func @array_box
  // CHECK: call @_RNvC{{[0-9]+}}REUSSIR_DROP_release_{{[A-Za-z0-9_]+}}
  // DISABLED-LABEL: func.func @array_box
  // DISABLED: reussir.rc.defer
  func.func @array_box(%ref: !reussir.ref<!reussir.rc<!boxed>>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!reussir.rc<!boxed>>)
    return
  }
}

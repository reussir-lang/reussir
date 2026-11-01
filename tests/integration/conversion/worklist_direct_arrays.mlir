// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true' --verify-each | %FileCheck %s --implicit-check-not=reussir.drop.defer --implicit-check-not=reussir.rc.defer
!leaf = !reussir.record<compound "ArrayLeaf" { i64 }>
!direct = !reussir.record<compound "ArrayDirect" { !leaf }>
!outer = !reussir.record<compound "ArrayOuter" { !direct }>
!boxed = !reussir.array<? x !reussir.rc<!outer shared>>
!inlined = !reussir.array<? x !outer>
!atomic = !reussir.array<? x !reussir.nullable<!reussir.rc<!outer shared atomic>>>
module {
  // CHECK-LABEL: func.func @boxed
  // CHECK: scf.for
  func.func @boxed(%ref: !reussir.ref<!boxed>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!boxed>)
    return
  }
  // CHECK-LABEL: func.func @inlined
  // CHECK: scf.for
  func.func @inlined(%ref: !reussir.ref<!inlined>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!inlined>)
    return
  }
  // CHECK-LABEL: func.func @atomic
  // CHECK: scf.for
  // CHECK: reussir.rc.fetch_sub
  func.func @atomic(%ref: !reussir.ref<!atomic unspecified atomic>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!atomic unspecified atomic>)
    return
  }
  // CHECK-LABEL: func.func @array_box
  // CHECK: call @__reussir_drop.release.
  func.func @array_box(%ref: !reussir.ref<!reussir.rc<!boxed>>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!reussir.rc<!boxed>>)
    return
  }
}

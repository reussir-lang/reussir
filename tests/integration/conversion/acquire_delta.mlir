// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s --check-prefix=INLINE
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true' | %FileCheck %s --check-prefix=OUTLINE

!pair = !reussir.record<compound "DeltaPair" [value] { !reussir.rc<i32 atomic>, !reussir.rc<i32 atomic> }>
module {
  // INLINE-LABEL: func.func @pair
  // INLINE: reussir.rc.inc{{.*}} by %arg1
  // INLINE: reussir.rc.inc{{.*}} by %arg1
  // OUTLINE: func.func private @{{.*}}DeltaPair{{.*}}(%{{.*}}: !reussir.ref<!reussir.record<compound "DeltaPair"{{.*}}>>, %arg1: index)
  // OUTLINE: reussir.rc.inc{{.*}} by %arg1
  // OUTLINE: reussir.rc.inc{{.*}} by %arg1
  // OUTLINE-LABEL: func.func @pair
  // OUTLINE: call @{{.*}}(%arg0, %arg1)
  func.func @pair(%ref: !reussir.ref<!pair>, %n: index) {
    reussir.ref.acquire(%ref : !reussir.ref<!pair>) by %n
    return
  }

  // INLINE-LABEL: func.func @nullable
  // INLINE: reussir.nullable.dispatch
  // INLINE: reussir.rc.inc{{.*}} by %arg1
  func.func @nullable(%ref: !reussir.ref<!reussir.nullable<!reussir.rc<i32>>>, %n: index) {
    reussir.ref.acquire(%ref : !reussir.ref<!reussir.nullable<!reussir.rc<i32>>>) by %n
    return
  }
}

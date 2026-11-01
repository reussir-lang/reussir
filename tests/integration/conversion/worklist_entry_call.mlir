// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --verify-each | %FileCheck %s
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=backward' --verify-each | %FileCheck %s

!node_ = !reussir.record<compound "EntryNode" incomplete>
!link = !reussir.nullable<!reussir.rc<!node_ shared>>
!node = !reussir.record<compound "EntryNode" { i64, !link, !link }>

// CHECK-LABEL: func.func private @__reussir_drop.entry.
// CHECK-SAME: [[ORDER:[a-z]+]](
// CHECK-SAME: llvm.noalias
// CHECK-SAME: llvm.nonnull
// CHECK-SAME: llvm.noundef
// CHECK: reussir.drop.run @__reussir_drop.walk.
// CHECK: return
// CHECK-LABEL: func.func @drop_root
// CHECK-NOT: reussir.drop.run
// CHECK: call @__reussir_drop.entry.[[ORDER]]
// CHECK-NEXT: return
module {
  func.func @drop_root(%ref: !reussir.ref<!node>) {
    reussir.ref.drop(%ref : !reussir.ref<!node>)
    return
  }
}

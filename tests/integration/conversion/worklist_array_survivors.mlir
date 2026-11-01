// RUN: %reussir-opt %s --split-input-file --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --verify-each | %FileCheck %s

!handle = !reussir.rc<!reussir.ffi_object<"ArrayHandle", @handle_drop>>
!array = !reussir.array<? x !handle>

module {
  func.func private @handle_drop(!handle)
  func.func @drop_array(%ref: !reussir.ref<!array>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!array>)
    return
  }
}

// CHECK-LABEL: func.func private @_RNvC{{[0-9]+}}REUSSIR_DROP_array_forward{{[A-Za-z0-9_]+}}(
// CHECK: scf.while
// CHECK: scf.condition
// CHECK: reussir.ref.load
// CHECK: reussir.rc.fetch
// CHECK: %[[DEAD:.*]] = arith.cmpi eq
// CHECK: reussir.rc.set
// CHECK: scf.if %[[DEAD]] {
// CHECK-NEXT: reussir.drop.defer_indexed
// CHECK-NEXT: reussir.drop.defer @_RNvC{{[0-9]+}}REUSSIR_DROP_release_forward{{[A-Za-z0-9_]+}}
// CHECK: arith.select %[[DEAD]]
// CHECK: scf.yield

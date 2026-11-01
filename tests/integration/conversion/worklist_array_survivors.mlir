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

// -----

!handle = !reussir.rc<!reussir.ffi_object<"ArrayHandle", @handle_drop> shared atomic>
!nullable = !reussir.nullable<!handle>
!array = !reussir.array<2 x ? x !nullable>

module {
  func.func private @handle_drop(!handle)
  func.func @drop_nullable_array(%ref: !reussir.ref<!array unspecified atomic>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!array unspecified atomic>)
    return
  }
}

// CHECK-LABEL: func.func private @_RNvC{{[0-9]+}}REUSSIR_DROP_array_forward{{[A-Za-z0-9_]+}}(
// CHECK: scf.while
// CHECK: scf.condition
// CHECK: reussir.array.project
// CHECK: reussir.array.project
// CHECK: reussir.nullable.check
// CHECK: reussir.rc.fetch_sub
// CHECK: scf.if
// CHECK-NEXT: reussir.drop.defer_indexed
// CHECK-NEXT: {{.*}}reussir.nullable.coerce
// CHECK-NEXT: reussir.drop.defer @_RNvC{{[0-9]+}}REUSSIR_DROP_release_forward{{[A-Za-z0-9_]+}}
// CHECK: arith.select
// CHECK: scf.yield

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

// CHECK-LABEL: func.func private @__reussir_drop.array.forward(
// CHECK: scf.while
// CHECK: scf.condition
// CHECK: reussir.ref.load
// CHECK: reussir.rc.fetch
// CHECK: %[[DEAD:.*]] = arith.cmpi eq
// CHECK: reussir.rc.set
// CHECK: scf.if %[[DEAD]] {
// CHECK-NEXT: reussir.drop.defer_indexed
// CHECK-NEXT: reussir.drop.defer @__reussir_drop.release.forward
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

// CHECK-LABEL: func.func private @__reussir_drop.array.forward(
// CHECK: scf.while
// CHECK: scf.condition
// CHECK: reussir.array.project
// CHECK: reussir.array.project
// CHECK: reussir.nullable.check
// CHECK: reussir.rc.fetch_sub
// CHECK: scf.if
// CHECK-NEXT: reussir.drop.defer_indexed
// CHECK-NEXT: {{.*}}reussir.nullable.coerce
// CHECK-NEXT: reussir.drop.defer @__reussir_drop.release.forward
// CHECK: arith.select
// CHECK: scf.yield

// -----

!array = !reussir.array<? x !reussir.rc<i64>>
module {
  func.func @drop_leaves(%ref: !reussir.ref<!array>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!array>)
    return
  }
}
// CHECK-LABEL: func.func @drop_leaves
// CHECK-NOT: reussir.drop.defer
// CHECK: scf.for
// CHECK: reussir.rc.fetch
// CHECK: reussir.token.free
// CHECK-NOT: reussir.drop.defer
// CHECK: return

// -----

!leaf = !reussir.rc<i64 shared atomic>
!array = !reussir.array<2 x ? x !reussir.nullable<!leaf>>
module {
  func.func @drop_nullable_leaves(%ref: !reussir.ref<!array unspecified atomic>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!array unspecified atomic>)
    return
  }
}
// CHECK-LABEL: func.func @drop_nullable_leaves
// CHECK-NOT: reussir.drop.defer
// CHECK: scf.for
// CHECK: scf.for
// CHECK: reussir.nullable.dispatch
// CHECK: reussir.rc.fetch_sub
// CHECK: reussir.token.free
// CHECK-NOT: reussir.drop.defer
// CHECK: return

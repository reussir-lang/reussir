// RUN: %reussir-opt %s --split-input-file --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true' --verify-each | %FileCheck %s --implicit-check-not=' cf.'
// RUN: %reussir-opt %s --split-input-file --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},func.func(reussir-token-reuse),reussir-convert-to-std,canonicalize,expand-strided-metadata,lower-affine,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' | %reussir-translate --split-input-file --mlir-to-llvmir | %FileCheck %s --check-prefix=LLVM

!node_ = !reussir.record<compound "AtomicNode" incomplete>
!node = !reussir.record<compound "AtomicNode" { !reussir.nullable<!reussir.rc<!node_ shared atomic>> }>
// CHECK-LABEL: func.func private @__reussir_drop.walk.forward
// CHECK: reussir.rc.fetch_sub
// CHECK-NOT: reussir.rc.set
// CHECK: reussir.rc.defer
// CHECK-NOT: reussir.token.free
// CHECK: return
// CHECK-LABEL: func.func private @__reussir_drop.release.forward(
// CHECK: reussir.drop.defer @__reussir_drop.free.forward
// CHECK-NEXT: call {{.*}}reussir.musttail
// CHECK-NEXT: return
// CHECK-LABEL: func.func @atomic_release
// CHECK: reussir.drop.run
module {
  func.func @atomic_release(%ref: !reussir.ref<!reussir.rc<!node shared atomic>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.rc<!node shared atomic>>)
    return
  }
}

// -----

!node_ = !reussir.record<compound "ArrayNode" incomplete>
!node = !reussir.record<compound "ArrayNode" { !reussir.array<1x!reussir.nullable<!reussir.rc<!node_ shared>>> }>
// CHECK-LABEL: func.func private @__reussir_drop.array.forward
// CHECK: reussir.array.view
// CHECK: reussir.drop.defer_indexed
// CHECK: reussir.array.project
// CHECK: reussir.ref.load
// CHECK-LABEL: func.func private @__reussir_drop.release.forward
// CHECK: reussir.drop.defer @__reussir_drop.free.forward
// CHECK-NEXT: call {{.*}}reussir.musttail
// CHECK-NEXT: return
// CHECK-LABEL: func.func @array_release
// CHECK: reussir.drop.run
module {
  func.func @array_release(%ref: !reussir.ref<!reussir.rc<!node shared>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.rc<!node shared>>)
    return
  }
}

// -----

!node_ = !reussir.record<compound "CellNode" incomplete>
!node = !reussir.record<compound "CellNode" { !reussir.cell<!reussir.nullable<!reussir.rc<!node_ shared>>> }>
// CHECK-LABEL: func.func private @__reussir_drop.walk.forward
// CHECK: reussir.ref.project
// CHECK: reussir.ref.load
// CHECK: reussir.rc.defer
// CHECK-NOT: reussir.token.free
// CHECK: return
// CHECK-LABEL: func.func private @__reussir_drop.release.forward
// CHECK: reussir.drop.defer @__reussir_drop.free.forward
// CHECK-NEXT: call {{.*}}reussir.musttail
// CHECK-NEXT: return
// CHECK-LABEL: func.func @cell_release
// CHECK: reussir.drop.run
module {
  func.func @cell_release(%ref: !reussir.ref<!reussir.rc<!node shared>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.rc<!node shared>>)
    return
  }
}

// LLVM-LABEL: define private void @__reussir_drop.release.forward(
// LLVM: call void @__reussir_drop_defer
// LLVM-NEXT: musttail call void @__reussir_drop.walk.forward
// LLVM-NEXT: ret void
// LLVM-LABEL: define void @atomic_release(
// LLVM: call void @__reussir_drop_run
// LLVM-LABEL: define private void @__reussir_drop.array.forward(
// LLVM: call void @__reussir_drop_defer_indexed
// LLVM-LABEL: define private void @__reussir_drop.release.forward
// LLVM: call void @__reussir_drop_defer
// LLVM-NEXT: musttail call void @__reussir_drop.walk.forward
// LLVM-NEXT: ret void
// LLVM-LABEL: define void @array_release(
// LLVM: call void @__reussir_drop_run
// LLVM-LABEL: define private void @__reussir_drop.release.forward
// LLVM: call void @__reussir_drop_defer
// LLVM-NEXT: musttail call void @__reussir_drop.walk.forward
// LLVM-NEXT: ret void
// LLVM-LABEL: define void @cell_release(
// LLVM: call void @__reussir_drop_run

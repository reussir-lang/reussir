// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true' --verify-each | %FileCheck %s --check-prefix=EXPAND
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},func.func(reussir-token-reuse),reussir-convert-to-std,canonicalize,expand-strided-metadata,lower-affine,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' | %reussir-translate --mlir-to-llvmir | %FileCheck %s --check-prefix=LLVM
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true specialize-recursive-drops=false' --verify-each | %FileCheck %s --check-prefix=PLAIN --implicit-check-not=worklist --implicit-check-not=enqueue_dtor

!node_ = !reussir.record<compound "Branch" incomplete>
!node = !reussir.record<compound "Branch" { !reussir.nullable<!reussir.rc<!node_ shared>>, !reussir.nullable<!reussir.rc<!node_ shared>> }>
!dynamic_ = !reussir.record<compound "DynamicBranch" incomplete>
!children = !reussir.array<? x 2 x !reussir.nullable<!reussir.rc<!dynamic_>>>
!dynamic = !reussir.record<compound "DynamicBranch" { !children }>
module {
  // EXPAND-LABEL: func.func @release
  // EXPAND: call @{{.*}}BranchE.worklist_release
  // PLAIN-LABEL: func.func @release
  // PLAIN: reussir.rc.fetch
  func.func @release(%ref: !reussir.ref<!reussir.rc<!node shared>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.rc<!node shared>>)
    return
  }
  // EXPAND-LABEL: func.func @release_dynamic
  // EXPAND: call @{{.*}}DynamicBranchE.worklist_release
  func.func @release_dynamic(%ref: !reussir.ref<!reussir.rc<!dynamic>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.rc<!dynamic>>)
    return
  }
}
// LLVM-LABEL: define linkonce_odr void @{{.*}}BranchE.worklist_release(
// LLVM: call i64 @__reussir_dtor_worklist_size()
// LLVM: call i64 @__reussir_dtor_worklist_align()
// LLVM: alloca i8, i64
// LLVM: ptrtoint
// LLVM: and i64
// LLVM: inttoptr
// LLVM: call ptr @__reussir_create_dtor_worklist
// LLVM: call void @{{.*}}BranchE.worklist_release.step.
// LLVM: call void @__reussir_drain_dtor_worklist
// LLVM-NEXT: call void @__reussir_destroy_dtor_worklist
// LLVM-NEXT: ret void

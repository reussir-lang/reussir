// RUN: %reussir-opt %s --split-input-file --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true drop-order=forward' --verify-each | %FileCheck %s
!tree_ = !reussir.record<compound "BranchChild" incomplete>
!tree = !reussir.record<compound "BranchChild" { !tree_, !tree_ }>
!list_ = !reussir.record<compound "TailWithBranch" incomplete>
!list = !reussir.record<compound "TailWithBranch" { !tree, !list_ }>
!wrapper = !reussir.record<compound "BranchWrapper" [value] { !list }>
!array = !reussir.array<? x !wrapper>
module {
  // CHECK-LABEL: func.func @contains_worklist
  // CHECK: reussir.drop.defer_indexed
  // CHECK: return
  func.func @contains_worklist(%ref: !reussir.ref<!array>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!array>)
    return
  }
}

// -----
!list_ = !reussir.record<compound "WasmList" incomplete>
!list = !reussir.record<compound "WasmList" { !reussir.nullable<!reussir.rc<!list_>> }>
!array = !reussir.array<? x !reussir.rc<!list>>
// CHECK-NOT: tail_release
module attributes {llvm.target_triple = "wasm32-unknown-unknown"} {
  // CHECK-LABEL: func.func @wasm_array
  // CHECK: reussir.drop.defer_indexed
  func.func @wasm_array(%ref: !reussir.ref<!array>) attributes {reussir.worklist_drop} {
    reussir.ref.drop(%ref : !reussir.ref<!array>)
    return
  }
}

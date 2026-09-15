// RUN: %reussir-opt %s --convert-to-llvm | %FileCheck %s

// A dynamically sized token (`token<align, ?>`) takes its byte size as an
// SSA operand and always allocates through the generic entry point; the
// `_small` fast path needs a compile-time-constant size. Freeing it stays
// the unsized path the allocator already provides.
module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  // CHECK-LABEL: llvm.func @token_alloc_dynamic(
  // CHECK: %[[ALIGN:.+]] = llvm.mlir.constant(16 : [[INDEX_T:i[0-9]+]]) : [[INDEX_T]]
  // CHECK: %[[TOK:.+]] = llvm.call @__reussir_allocate(%[[ALIGN]], %arg0) : ([[INDEX_T]], [[INDEX_T]]) -> !llvm.ptr
  // CHECK: llvm.return %[[TOK]] : !llvm.ptr
  func.func private @token_alloc_dynamic(%bytes: index)
    -> !reussir.token<align: 16, size: ?> {
      %token = reussir.token.alloc(%bytes : index) : !reussir.token<align: 16, size: ?>
      return %token : !reussir.token<align: 16, size: ?>
  }

  // CHECK-LABEL: llvm.func @token_free_dynamic(%arg0: !llvm.ptr)
  // CHECK: llvm.call @__reussir_dealloc_unsized(%arg0) : (!llvm.ptr) -> ()
  // CHECK: llvm.return
  func.func private @token_free_dynamic(%token: !reussir.token<align: 16, size: ?>) {
      reussir.token.free (%token : !reussir.token<align: 16, size: ?>)
      return
  }
}

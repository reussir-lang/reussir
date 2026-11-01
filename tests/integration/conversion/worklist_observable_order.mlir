// REQUIRES: pointer64
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion{drop-order=forward},reussir-acquire-drop-expansion,reussir-convert-to-std{expand-arrays=false},func.func(reussir-inc-dec-cancellation),reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},func.func(reussir-token-reuse),reussir-convert-to-std,canonicalize,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' -o %t.forward.mlir
// RUN: %reussir-translate --reussir-to-llvmir %t.forward.mlir | %opt -S -O2 -o %t.forward.ll
// RUN: %llc %t.forward.ll -relocation-model=pic -filetype=obj -o %t.forward.o
// RUN: %cc %t.forward.o %S/worklist_observable_order.c -o %t.forward.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.forward.exe forward
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion{drop-order=backward},reussir-acquire-drop-expansion,reussir-convert-to-std{expand-arrays=false},func.func(reussir-inc-dec-cancellation),reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},func.func(reussir-token-reuse),reussir-convert-to-std,canonicalize,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' -o %t.backward.mlir
// RUN: %reussir-translate --reussir-to-llvmir %t.backward.mlir | %opt -S -O2 -o %t.backward.ll
// RUN: %llc %t.backward.ll -relocation-model=pic -filetype=obj -o %t.backward.o
// RUN: %cc %t.backward.o %S/worklist_observable_order.c -o %t.backward.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.backward.exe backward

!handle = !reussir.rc<!reussir.ffi_object<"OrderHandle", @order_handle_drop>>
!leaf = !reussir.record<compound "OrderLeaf" { !handle }>
!node = !reussir.record<compound "OrderNode" { !leaf, !handle, !leaf }>
!y = !reussir.record<compound "OrderY" { !leaf, !handle }>
!after = !reussir.record<compound "OrderAfter" { !leaf, !y }>
!before = !reussir.record<compound "OrderBefore" { !y, !leaf }>
!pair = !reussir.record<compound "OrderPair" { !handle, !handle }>
!small_token = !reussir.nullable<!reussir.token<align: 8, size: 16>>
!pair_token = !reussir.nullable<!reussir.token<align: 8, size: 24>>
!node_token = !reussir.nullable<!reussir.token<align: 8, size: 32>>

module {
  func.func private @order_handle_drop(!handle)

  func.func @order_replace_pair(%old: !reussir.rc<!pair>, %a: !handle, %b: !handle) -> !reussir.rc<!pair> {
    %donor = reussir.rc.dec(%old : !reussir.rc<!pair>) : !pair_token
    %value = reussir.record.compound(%a, %b : !handle, !handle) : !pair
    %token = reussir.token.alloc : !reussir.token<align: 8, size: 24>
    %new = reussir.rc.create value(%value : !pair) token(%token : !reussir.token<align: 8, size: 24>) : !reussir.rc<!pair>
    return %new : !reussir.rc<!pair>
  }

  func.func @order_drop_pair(%rc: !reussir.rc<!pair>) {
    %token = reussir.rc.dec(%rc : !reussir.rc<!pair>) : !pair_token
    reussir.token.free(%token : !pair_token)
    return
  }

  func.func @order_drop_leaf(%rc: !reussir.rc<!leaf>) {
    %token = reussir.rc.dec(%rc : !reussir.rc<!leaf>) : !small_token
    reussir.token.free(%token : !small_token)
    return
  }
  func.func @order_drop_node(%rc: !reussir.rc<!node>) {
    %token = reussir.rc.dec(%rc : !reussir.rc<!node>) : !node_token
    reussir.token.free(%token : !node_token)
    return
  }
  func.func @order_drop_after(%rc: !reussir.rc<!after>) {
    %token = reussir.rc.dec(%rc : !reussir.rc<!after>) : !pair_token
    reussir.token.free(%token : !pair_token)
    return
  }
  func.func @order_drop_before(%rc: !reussir.rc<!before>) {
    %token = reussir.rc.dec(%rc : !reussir.rc<!before>) : !pair_token
    reussir.token.free(%token : !pair_token)
    return
  }
}

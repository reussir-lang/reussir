// UNSUPPORTED: windows
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},func.func(reussir-token-reuse),reussir-convert-to-std,canonicalize,expand-strided-metadata,lower-affine,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' | %reussir-translate --mlir-to-llvmir > %t.ll
// RUN: %llc %t.ll -O0 -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/worklist_drop_order.c -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe
// RUN: %opt -S -O3 %t.ll -o %t.opt.ll
// RUN: %llc %t.opt.ll -relocation-model=pic -filetype=obj -o %t.opt.o
// RUN: %cc %t.opt.o %S/worklist_drop_order.c -o %t.opt.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.opt.exe
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-acquire-drop-expansion{outline-record=true expand-decrement=true specialize-recursive-drops=false},func.func(reussir-token-reuse),reussir-convert-to-std,canonicalize,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' | %reussir-translate --mlir-to-llvmir > %t.plain.ll
// RUN: %llc %t.plain.ll -O0 -relocation-model=pic -filetype=obj -o %t.plain.o
// RUN: %cc -DSHALLOW %t.plain.o %S/worklist_drop_order.c -o %t.plain.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.plain.exe

!hook = !reussir.rc<!reussir.ffi_object<"Hook", @order_note>>
!tail = !reussir.record<compound "InlineTail" [value] { !hook }>
!node_ = !reussir.record<compound "Ordered" incomplete>
!next = !reussir.nullable<!reussir.rc<!node_>>
!node = !reussir.record<compound "Ordered" { !hook, !next, !tail }>
!owner = !reussir.rc<!node>
module {
  func.func private @order_note(!hook)
  func.func @order_make(%next: !next, %pre: !hook, %post: !hook) -> !owner {
    %tail = reussir.record.compound(%post : !hook) : !tail
    %payload = reussir.record.compound(%pre, %next, %tail : !hook, !next, !tail) : !node
    %owner = reussir.rc.create value(%payload : !node) : !owner
    return %owner : !owner
  }
  func.func @order_drop(%ref: !reussir.ref<!owner>) {
    reussir.ref.drop (%ref : !reussir.ref<!owner>)
    return
  }
}

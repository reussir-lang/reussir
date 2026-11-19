// REQUIRES: pointer64
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-acquire-drop-expansion{outline-record=true expand-decrement=true drop-order=forward},reussir-convert-to-std,canonicalize,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' -o %t.forward.mlir
// RUN: %reussir-translate --reussir-to-llvmir %t.forward.mlir | %opt -S -O2 -o %t.forward.ll
// RUN: %llc %t.forward.ll -relocation-model=pic -filetype=obj -o %t.forward.o
// RUN: %cc %t.forward.o %S/worklist_pure_order.c -o %t.forward.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.forward.exe forward
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-acquire-drop-expansion{outline-record=true expand-decrement=true drop-order=backward},reussir-convert-to-std,canonicalize,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' -o %t.backward.mlir
// RUN: %reussir-translate --reussir-to-llvmir %t.backward.mlir | %opt -S -O2 -o %t.backward.ll
// RUN: %llc %t.backward.ll -relocation-model=pic -filetype=obj -o %t.backward.o
// RUN: %cc %t.backward.o %S/worklist_pure_order.c -o %t.backward.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.backward.exe backward

// A record of boxes decrements them all and continues in the last dead one:
// the hooks behind them keep their order, and a long chain needs no stack.

!handle = !reussir.rc<!reussir.ffi_object<"PureHandle", @pure_cleanup>>
!owner = !reussir.record<compound "PureOwner" { !handle }>
!node_ = !reussir.record<compound "PureOrder" incomplete>
!link = !reussir.nullable<!reussir.rc<!node_ shared>>
!node = !reussir.record<compound "PureOrder" { !owner, !link, !link }>
!rc = !reussir.rc<!node shared>

module {
  func.func private @pure_cleanup(!handle)
  func.func @pure_drop(%rc: !rc) {
    %token = reussir.rc.dec(%rc : !rc) : !reussir.nullable<!reussir.token<align: 8, size: 32>>
    reussir.token.free(%token : !reussir.nullable<!reussir.token<align: 8, size: 32>>)
    return
  }
}

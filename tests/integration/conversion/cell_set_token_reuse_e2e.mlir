// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion{drop-order=forward},reussir-acquire-drop-expansion,reussir-convert-to-std{expand-arrays=false},reussir-acquire-drop-expansion{outline-record=false expand-decrement=false tail-recursive-drops=false},func.func(reussir-inc-dec-cancellation),func.func(reussir-token-reuse))' -o %t.forward.reuse.mlir
// RUN: %FileCheck %s < %t.forward.reuse.mlir
// RUN: %reussir-opt %t.forward.reuse.mlir --pass-pipeline='builtin.module(reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},reussir-convert-to-std,canonicalize,cse,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' -o %t.forward.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.forward.mlir | %opt -S -O2 -o %t.forward.ll
// RUN: %llc %t.forward.ll -relocation-model=pic -filetype=obj -o %t.forward.o
// RUN: %cc %t.forward.o -o %t.forward.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.forward.exe
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion{drop-order=backward},reussir-acquire-drop-expansion,reussir-convert-to-std{expand-arrays=false},reussir-acquire-drop-expansion{outline-record=false expand-decrement=false tail-recursive-drops=false},func.func(reussir-inc-dec-cancellation),func.func(reussir-token-reuse))' -o %t.backward.reuse.mlir
// RUN: %FileCheck %s < %t.backward.reuse.mlir
// RUN: %reussir-opt %t.backward.reuse.mlir --pass-pipeline='builtin.module(reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},reussir-convert-to-std,canonicalize,cse,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' -o %t.backward.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.backward.mlir | %opt -S -O2 -o %t.backward.ll
// RUN: %llc %t.backward.ll -relocation-model=pic -filetype=obj -o %t.backward.o
// RUN: %cc %t.backward.o -o %t.backward.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.backward.exe

!rc = !reussir.rc<i64>
!cell = !reussir.rc<!reussir.cell<!rc>>
!ref = !reussir.ref<i64>
module {
  // CHECK-LABEL: func.func @replace_cell
  // CHECK: %[[DONOR:[^ ]+]] = reussir.rc.dec
  // CHECK-NOT: reussir.token.alloc
  // CHECK: %[[TOKEN:[^ ]+]] = reussir.token.ensure(%[[DONOR]]
  // CHECK: reussir.rc.create {{.*}}token(%[[TOKEN]]
  func.func @replace_cell(%cell: !cell, %replacement: !rc, %value: i64) -> !rc {
    reussir.cell.set(%replacement : !rc, %cell : !cell)
    %new = reussir.rc.create value(%value : i64) : !rc
    return %new : !rc
  }

  func.func @main() -> i32 {
    %one = arith.constant 1 : i64
    %two = arith.constant 2 : i64
    %three = arith.constant 3 : i64
    %old = reussir.rc.create value(%one : i64) : !rc
    %before = reussir.rc.borrow(%old : !rc) : !ref
    %unique_cell = reussir.cell.create value(%old : !rc) : !cell
    %replacement = reussir.rc.create value(%two : i64) : !rc
    %new = func.call @replace_cell(%unique_cell, %replacement, %three) : (!cell, !rc, i64) -> !rc
    %after = reussir.rc.borrow(%new : !rc) : !ref
    %different = reussir.ref.cmp ne %before, %after : (!ref, !ref) -> i1
    %new_value = reussir.ref.load(%after : !ref) : i64
    %wrong = arith.cmpi ne, %new_value, %three : i64
    %failed = arith.ori %different, %wrong : i1
    scf.if %failed {
      reussir.panic "CellSet did not reuse the unique payload allocation"
    }
    reussir.rc.dec(%unique_cell : !cell)
    reussir.rc.dec(%new : !rc)

    %shared = reussir.rc.create value(%one : i64) : !rc
    reussir.rc.inc(%shared : !rc)
    %shared_before = reussir.rc.borrow(%shared : !rc) : !ref
    %shared_cell = reussir.cell.create value(%shared : !rc) : !cell
    %shared_replacement = reussir.rc.create value(%two : i64) : !rc
    %fresh = func.call @replace_cell(%shared_cell, %shared_replacement, %three) : (!cell, !rc, i64) -> !rc
    %fresh_ref = reussir.rc.borrow(%fresh : !rc) : !ref
    %same = reussir.ref.cmp eq %shared_before, %fresh_ref : (!ref, !ref) -> i1
    %old_value = reussir.ref.load(%shared_before : !ref) : i64
    %changed = arith.cmpi ne, %old_value, %one : i64
    %shared_failed = arith.ori %same, %changed : i1
    scf.if %shared_failed {
      reussir.panic "CellSet reused a payload with a retained alias"
    }
    reussir.rc.dec(%shared_cell : !cell)
    reussir.rc.dec(%shared : !rc)
    reussir.rc.dec(%fresh : !rc)
    %zero = arith.constant 0 : i32
    return %zero : i32
  }
}

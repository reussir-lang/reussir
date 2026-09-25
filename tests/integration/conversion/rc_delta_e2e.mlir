// RUN: %reussir-opt %s \
// RUN:   --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-closure-outlining,reussir-lowering-region-patterns,func.func(reussir-inc-dec-cancellation),reussir-rc-decrement-expansion,func.func(reussir-infer-variant-tag),reussir-acquire-drop-expansion,reussir-convert-to-std,func.func(reussir-inc-dec-cancellation),reussir-acquire-drop-expansion{expand-decrement=1 outline-record=1},func.func(reussir-token-reuse),reussir-convert-to-std,func.func(reussir-rc-create-sink),func.func(reussir-rc-create-fusion),reussir-trmc-recursion-analysis,reussir-compile-polymorphic-ffi,canonicalize,cse,control-flow-sink,convert-scf-to-cf,reussir-lowering-basic-ops,convert-to-llvm,reconcile-unrealized-casts,cse,canonicalize)' \
// RUN:   -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -o %t.exe -L%library_path -lreussir_rt \
// RUN:   %rpath_flag %extra_sys_libs
// RUN: %t.exe

module {
  func.func @normal(%n: index) {
    %value = arith.constant 42 : i32
    %rc = reussir.rc.create value(%value : i32) : !reussir.rc<i32>
    reussir.rc.inc(%rc : !reussir.rc<i32>) by %n
    %one = arith.constant 1 : index
    %expected = arith.addi %n, %one : index
    %count = reussir.rc.fetch(%rc : !reussir.rc<i32>) : index
    %bad = arith.cmpi ne, %count, %expected : index
    scf.if %bad {
      reussir.panic "incorrect delta acquisition"
    }
    %zero = arith.constant 0 : index
    scf.for %i = %zero to %n step %one {
      reussir.rc.dec(%rc : !reussir.rc<i32>)
    }
    reussir.rc.dec(%rc : !reussir.rc<i32>)
    return
  }
  func.func @atomic(%n: index) {
    %value = arith.constant 42 : i32
    %rc = reussir.rc.create value(%value : i32) : !reussir.rc<i32 atomic>
    reussir.rc.inc(%rc : !reussir.rc<i32 atomic>) by %n
    %one = arith.constant 1 : index
    %expected = arith.addi %n, %one : index
    %count = reussir.rc.fetch(%rc : !reussir.rc<i32 atomic>) : index
    %bad = arith.cmpi ne, %count, %expected : index
    scf.if %bad {
      reussir.panic "incorrect delta acquisition"
    }
    %zero = arith.constant 0 : index
    scf.for %i = %zero to %n step %one {
      reussir.rc.dec(%rc : !reussir.rc<i32 atomic>)
    }
    reussir.rc.dec(%rc : !reussir.rc<i32 atomic>)
    return
  }
  func.func @main() -> i32 {
    %five = arith.constant 5 : index
    %zero = arith.constant 0 : index
    func.call @normal(%five) : (index) -> ()
    func.call @normal(%zero) : (index) -> ()
    func.call @atomic(%five) : (index) -> ()
    func.call @atomic(%zero) : (index) -> ()
    %result = arith.constant 0 : i32
    return %result : i32
  }
}

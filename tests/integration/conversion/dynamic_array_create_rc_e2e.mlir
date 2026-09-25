// RUN: %reussir-opt %s \
// RUN:   --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-closure-outlining,reussir-lowering-region-patterns,func.func(reussir-inc-dec-cancellation),reussir-rc-decrement-expansion,func.func(reussir-infer-variant-tag),reussir-acquire-drop-expansion,reussir-convert-to-std,func.func(reussir-inc-dec-cancellation),reussir-acquire-drop-expansion{expand-decrement=1 outline-record=1},func.func(reussir-token-reuse),reussir-convert-to-std,func.func(reussir-rc-create-sink),func.func(reussir-rc-create-fusion),reussir-trmc-recursion-analysis,reussir-compile-polymorphic-ffi,canonicalize,cse,control-flow-sink,convert-scf-to-cf,reussir-lowering-basic-ops,convert-to-llvm,reconcile-unrealized-casts,cse,canonicalize)' \
// RUN:   -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -o %t.exe -L%library_path -lreussir_rt \
// RUN:   %rpath_flag %extra_sys_libs
// RUN: %t.exe

!elem = !reussir.rc<i32>
!array = !reussir.array<? x !elem>
!rc_array = !reussir.rc<!array>
module {
  func.func private @check_clones(%n: index) attributes {llvm.linkage = #llvm.linkage<internal>} {
    %value = arith.constant 42 : i32
    %init = reussir.rc.create value(%value : i32) : !elem
    %array = reussir.array.create extents(%n) : !rc_array body {
      ^bb0(%array_i0: index):
        reussir.rc.inc(%init : !elem)
        reussir.scf.yield %init : !elem
    }
    %one = arith.constant 1 : index
    %expected = arith.addi %n, %one : index
    %count = reussir.rc.fetch(%init : !elem) : index
    %bad = arith.cmpi ne, %count, %expected : index
    scf.if %bad {
      reussir.panic "array constructor did not clone each element"
    }
    %ref = reussir.rc.borrow(%array : !rc_array) : !reussir.ref<!array>
    %view = reussir.array.view(%ref : !reussir.ref<!array>) : memref<?x!elem, strided<[?], offset: ?>>
    %zero = arith.constant 0 : index
    scf.for %i = %zero to %n step %one {
      %element = memref.load %view[%i] : memref<?x!elem, strided<[?], offset: ?>>
      %element_ref = reussir.rc.borrow(%element : !elem) : !reussir.ref<i32>
      %loaded = reussir.ref.load(%element_ref : !reussir.ref<i32>) : i32
      %bad_value = arith.cmpi ne, %loaded, %value : i32
      scf.if %bad_value {
        reussir.panic "array slot contains the wrong element"
      }
      reussir.rc.dec(%element : !elem)
    }
    %token = reussir.rc.reinterpret(%array : !rc_array) : !reussir.token<align: 8, size: ?>
    reussir.token.free(%token : !reussir.token<align: 8, size: ?>)
    %remaining = reussir.rc.fetch(%init : !elem) : index
    %bad_remaining = arith.cmpi ne, %remaining, %one : index
    scf.if %bad_remaining {
      reussir.panic "array construction consumed the initializer"
    }
    reussir.rc.dec(%init : !elem)
    return
  }

  func.func @main() -> i32 {
    %three = arith.constant 3 : index
    func.call @check_clones(%three) : (index) -> ()
    %zero = arith.constant 0 : index
    func.call @check_clones(%zero) : (index) -> ()
    %result = arith.constant 0 : i32
    return %result : i32
  }
}

// RUN: %reussir-opt %s \
// RUN:   --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-closure-outlining,reussir-lowering-region-patterns,func.func(reussir-inc-dec-cancellation),reussir-rc-decrement-expansion,func.func(reussir-infer-variant-tag),reussir-acquire-drop-expansion,reussir-convert-to-std,func.func(reussir-inc-dec-cancellation),reussir-acquire-drop-expansion{expand-decrement=1 outline-record=1},func.func(reussir-token-reuse),reussir-convert-to-std,func.func(reussir-rc-create-sink),func.func(reussir-rc-create-fusion),reussir-trmc-recursion-analysis,reussir-compile-polymorphic-ffi,canonicalize,cse,control-flow-sink,expand-strided-metadata,lower-affine,canonicalize,convert-scf-to-cf,reussir-lowering-basic-ops,convert-to-llvm,reconcile-unrealized-casts,cse,canonicalize)' \
// RUN:   -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -o %t.exe -L%library_path -lreussir_rt \
// RUN:   %rpath_flag %extra_sys_libs
// RUN: %t.exe

// Executable proof of the ownership traversal over a dynamic-extent array of
// managed elements: a rank-2 `array<? x 2 x rc<i32>>` whose drop walks a loop
// nest bounded by `memref.dim` on the leading dim and projects through the
// dynamic strided layout at every rank. The array is shared, so the first
// decrement only lowers the count and the second releases every element
// exactly once.
!e = !reussir.rc<i32>
!dv = !reussir.array<? x 2 x !e>
!rc_dv = !reussir.rc<!dv>
!view = memref<?x2x!e, strided<[?, ?], offset: ?>>
!row = memref<2x!e, strided<[?], offset: ?>>
module {
  // Slot [i][j] holds rc(2i + j).
  func.func private @make(%n: index) -> !rc_dv attributes {llvm.linkage = #llvm.linkage<internal>} {
    %c2 = arith.constant 2 : index
    %rc = reussir.array.create extents(%n) : !rc_dv body {
      ^bb0(%i: index, %j: index):
        %i2 = arith.muli %i, %c2 : index
        %k = arith.addi %i2, %j : index
        %kv = arith.index_cast %k : index to i32
        %elem = reussir.rc.create value(%kv : i32) : !e
        reussir.scf.yield %elem : !e
    }
    return %rc : !rc_dv
  }

  func.func private @sum(%rc: !rc_dv) -> i32 attributes {llvm.linkage = #llvm.linkage<internal>} {
    %ref = reussir.rc.borrow (%rc : !rc_dv) : !reussir.ref<!dv>
    %v = reussir.array.view(%ref : !reussir.ref<!dv>) : !view
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %c2 = arith.constant 2 : index
    %n = memref.dim %v, %c0 : !view
    %c0i = arith.constant 0 : i32
    %s = scf.for %i = %c0 to %n step %c1 iter_args(%acc = %c0i) -> i32 {
      %row = reussir.array.project(%v : !view) [%i : index] : !row
      %s2 = scf.for %j = %c0 to %c2 step %c1 iter_args(%acc2 = %acc) -> i32 {
        %slot = reussir.array.project(%row : !row) [%j : index] : !reussir.ref<!e>
        %elem = reussir.ref.load (%slot : !reussir.ref<!e>) : !e
        %eref = reussir.rc.borrow (%elem : !e) : !reussir.ref<i32>
        %x = reussir.ref.load (%eref : !reussir.ref<i32>) : i32
        %a = arith.addi %acc2, %x : i32
        scf.yield %a : i32
      }
      scf.yield %s2 : i32
    }
    return %s : i32
  }

  func.func @main() -> i32 {
    %c5 = arith.constant 5 : index
    // sum(0..9) = 45
    %c45 = arith.constant 45 : i32

    %shared = func.call @make(%c5) : (index) -> !rc_dv
    reussir.rc.inc (%shared : !rc_dv)
    %shared_sum = func.call @sum(%shared) : (!rc_dv) -> i32
    %shared_bad = arith.cmpi ne, %shared_sum, %c45 : i32
    scf.if %shared_bad {
      reussir.panic "managed dynamic array sum mismatch"
    }
    // The first decrement only drops the count; the second walks the
    // traversal and releases every element exactly once.
    reussir.rc.dec (%shared : !rc_dv)
    reussir.rc.dec (%shared : !rc_dv)

    %c0 = arith.constant 0 : i32
    return %c0 : i32
  }
}

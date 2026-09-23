// RUN: %reussir-opt %s \
// RUN:   --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-closure-outlining,reussir-lowering-region-patterns,func.func(reussir-inc-dec-cancellation),reussir-rc-decrement-expansion,func.func(reussir-infer-variant-tag),reussir-acquire-drop-expansion,reussir-convert-to-std,func.func(reussir-inc-dec-cancellation),reussir-acquire-drop-expansion{expand-decrement=1 outline-record=1},func.func(reussir-token-reuse),reussir-convert-to-std,func.func(reussir-rc-create-sink),func.func(reussir-rc-create-fusion),reussir-trmc-recursion-analysis,reussir-compile-polymorphic-ffi,canonicalize,cse,control-flow-sink,convert-scf-to-cf,reussir-lowering-basic-ops,convert-to-llvm,reconcile-unrealized-casts,cse,canonicalize)' \
// RUN:   -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -o %t.exe -L%library_path -lreussir_rt \
// RUN:   %rpath_flag %extra_sys_libs
// RUN: %t.exe

!dv = !reussir.array<? x i32>
!rc_dv = !reussir.rc<!dv>
module {
  func.func private @splat_sum(%n: index, %init: i32) -> i32 attributes {llvm.linkage = #llvm.linkage<internal>} {
    %rc = reussir.array.create extents(%n) : !rc_dv body {
      ^bb0(%array_i0: index):
        reussir.scf.yield %init : i32
    }
    %ref = reussir.rc.borrow (%rc : !rc_dv) : !reussir.ref<!dv>
    %v = reussir.array.view(%ref : !reussir.ref<!dv>) : memref<?xi32, strided<[?], offset: ?>>
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %c0i = arith.constant 0 : i32
    %sum = scf.for %i = %c0 to %n step %c1 iter_args(%acc = %c0i) -> i32 {
      %x = memref.load %v[%i] : memref<?xi32, strided<[?], offset: ?>>
      %a = arith.addi %acc, %x : i32
      scf.yield %a : i32
    }
    reussir.rc.dec (%rc : !rc_dv)
    return %sum : i32
  }

  func.func private @mixed_sum(%n: index, %init: i32) -> i32 attributes {llvm.linkage = #llvm.linkage<internal>} {
    %rc = reussir.array.create extents(%n) : !reussir.rc<!reussir.array<? x 4 x i32>> body {
      ^bb0(%array_i0: index, %array_i1: index):
        reussir.scf.yield %init : i32
    }
    %ref = reussir.rc.borrow(%rc : !reussir.rc<!reussir.array<? x 4 x i32>>) : !reussir.ref<!reussir.array<? x 4 x i32>>
    %view = reussir.array.view(%ref : !reussir.ref<!reussir.array<? x 4 x i32>>) : memref<?x4xi32, strided<[?, ?], offset: ?>>
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    %four = arith.constant 4 : index
    %init_sum = arith.constant 0 : i32
    %sum = scf.for %i = %zero to %n step %one iter_args(%acc = %init_sum) -> i32 {
      %row = scf.for %j = %zero to %four step %one iter_args(%inner = %acc) -> i32 {
        %value = memref.load %view[%i, %j] : memref<?x4xi32, strided<[?, ?], offset: ?>>
        %next = arith.addi %inner, %value : i32
        scf.yield %next : i32
      }
      scf.yield %row : i32
    }
    reussir.rc.dec(%rc : !reussir.rc<!reussir.array<? x 4 x i32>>)
    return %sum : i32
  }

  func.func @main() -> i32 {
    %c10 = arith.constant 10 : index
    %c7 = arith.constant 7 : i32
    %s = func.call @splat_sum(%c10, %c7) : (index, i32) -> i32
    %c70 = arith.constant 70 : i32
    %bad = arith.cmpi ne, %s, %c70 : i32
    scf.if %bad {
      reussir.panic "dynamic array initialization mismatch"
    }
    %empty = arith.constant 0 : index
    %empty_sum = func.call @splat_sum(%empty, %c7) : (index, i32) -> i32
    %c0 = arith.constant 0 : i32
    %empty_bad = arith.cmpi ne, %empty_sum, %c0 : i32
    scf.if %empty_bad {
      reussir.panic "empty dynamic array sum mismatch"
    }
    %mixed = func.call @mixed_sum(%c10, %c7) : (index, i32) -> i32
    %c280 = arith.constant 280 : i32
    %mixed_bad = arith.cmpi ne, %mixed, %c280 : i32
    scf.if %mixed_bad {
      reussir.panic "mixed-shape initialization mismatch"
    }
    return %c0 : i32
  }
}

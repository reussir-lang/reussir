// REQUIRES: rcgmp
// RUN: %reussir-opt %s \
// RUN:   --pass-pipeline='builtin.module(reussir-attach-native-target,convert-rcgmp-to-scf,canonicalize,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' \
// RUN:   -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/rcgmp_e2e.c -o %t.exe -L%library_path -lreussir_rt \
// RUN:   %rcgmp_runtime %rpath_flag %extra_sys_libs
// RUN: %t.exe
// RUN: %rrc %s -o %t.rrc.o --relocation-mode pic
// RUN: %cc %t.rrc.o %S/rcgmp_e2e.c -o %t.rrc.exe -L%library_path -lreussir_rt \
// RUN:   %rcgmp_runtime %rpath_flag %extra_sys_libs
// RUN: %t.rrc.exe

// Arbitrary-precision integers cross function signatures, loop-carried block
// arguments and `scf.if` results, and outgrow the immediate range into heap
// integers allocated through the runtime hooks in reussir-rt.
module {
  func.func private @factorial(%n: i64) -> !rcgmp.mpz {
    %one = rcgmp.constant 1 : !rcgmp.mpz
    %c1 = arith.constant 1 : i64
    %end = arith.addi %n, %c1 : i64
    %r = scf.for %i = %c1 to %end step %c1 iter_args(%acc = %one) -> (!rcgmp.mpz) : i64 {
      %k = rcgmp.extsi %i : i64 to !rcgmp.mpz
      %next = rcgmp.muli %acc, %k : !rcgmp.mpz
      scf.yield %next : !rcgmp.mpz
    }
    return %r : !rcgmp.mpz
  }

  func.func @is_factorial_of_25(%n: i64) -> i1 {
    %f = func.call @factorial(%n) : (i64) -> !rcgmp.mpz
    %expected = rcgmp.constant 15511210043330985984000000 : !rcgmp.mpz
    %eq = rcgmp.cmpi eq, %f, %expected : !rcgmp.mpz
    rcgmp.release %f : !rcgmp.mpz
    return %eq : i1
  }

  // |a - b|, truncated to 64 bits.
  func.func @distance(%a: i64, %b: i64) -> i64 {
    %x = rcgmp.extsi %a : i64 to !rcgmp.mpz
    %y = rcgmp.extsi %b : i64 to !rcgmp.mpz
    %lt = rcgmp.cmpi slt, %x, %y : !rcgmp.mpz
    %d = scf.if %lt -> !rcgmp.mpz {
      %s = rcgmp.subi %y, %x : !rcgmp.mpz
      scf.yield %s : !rcgmp.mpz
    } else {
      %s = rcgmp.subi %x, %y : !rcgmp.mpz
      scf.yield %s : !rcgmp.mpz
    }
    %r = rcgmp.trunci %d : !rcgmp.mpz to i64
    rcgmp.release %d : !rcgmp.mpz
    return %r : i64
  }
}

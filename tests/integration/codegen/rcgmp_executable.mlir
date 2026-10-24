// REQUIRES: rcgmp, linux
// `rrc --emit executable` links the rcgmp integer runtime and GMP, staged
// beside the Reussir runtime, under both runtime linkages; the runtime's
// allocation hooks resolve against the Reussir runtime either way.

// RUN: %rrc %s -o %t.exe --emit executable --relocation-mode pic
// RUN: %t.exe
// RUN: %llvm-readobj --dynamic-table %t.exe | %FileCheck %s --check-prefix=RUNPATH
// RUNPATH: {{RUNPATH|RPATH}}
// RUN: %rrc %s -o %t.dynamic.exe --emit executable --relocation-mode pic --runtime-linkage dynamic --link-arg=-Wl,-rpath,%library_path
// RUN: %t.dynamic.exe

module {
  func.func private @exit(i32)

  // Exits with status 1 unless 30! matches its heap constant.
  func.func @__reussir_main() {
    %one = rcgmp.constant 1 : !rcgmp.mpz
    %c1 = arith.constant 1 : i64
    %c31 = arith.constant 31 : i64
    %f = scf.for %i = %c1 to %c31 step %c1 iter_args(%acc = %one) -> (!rcgmp.mpz) : i64 {
      %k = rcgmp.extsi %i : i64 to !rcgmp.mpz
      %next = rcgmp.muli %acc, %k : !rcgmp.mpz
      scf.yield %next : !rcgmp.mpz
    }
    %expected = rcgmp.constant 265252859812191058636308480000000 : !rcgmp.mpz
    %ne = rcgmp.cmpi ne, %f, %expected : !rcgmp.mpz
    rcgmp.release %f : !rcgmp.mpz
    scf.if %ne {
      %status = arith.constant 1 : i32
      func.call @exit(%status) : (i32) -> ()
    }
    return
  }
}

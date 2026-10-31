// RUN: %reussir-opt %s --reussir-convert-to-llvm --mlir-print-debuginfo --mlir-print-local-scope | %FileCheck %s

// With debug info, codegen fuses an exported trampoline's location with its
// `DBGSubprogramAttr`. The subprogram describes the generated C entry
// function alone; the entry's body takes the plain declaration position.

module attributes {llvm.target_triple = "x86_64-pc-linux-gnu"} {
  llvm.func @twice(%arg0: i64) -> i64 {
    %0 = llvm.add %arg0, %arg0 : i64
    llvm.return %0 : i64
  }

  reussir.trampoline export "C" @twice_ffi = @twice loc(#subprogram)
}

#decl = loc("lib.rr":7:1)
#subprogram = loc(fused<#reussir.dbg_subprogram<raw_name : "twice_ffi", type_params : []>>[#decl])

// CHECK-LABEL: llvm.func @twice_ffi(
// CHECK-NEXT: llvm.call @twice(%arg0) : (i64) -> i64 loc("lib.rr":7:1)
// CHECK-NEXT: llvm.return %{{.*}} : i64 loc("lib.rr":7:1)
// CHECK-NEXT: } loc(fused<#reussir.dbg_subprogram<raw_name : "twice_ffi", type_params : []>>["lib.rr":7:1])

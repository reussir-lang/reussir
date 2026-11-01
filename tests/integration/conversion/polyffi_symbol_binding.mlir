// RUN: %reussir-opt %s --symbol-dce --verify-each | %FileCheck %s
// RUN: %reussir-opt %s --emit-bytecode -o %t.bc
// RUN: %reussir-opt %t.bc --symbol-dce --verify-each | %FileCheck %s
// RUN: %reussir-opt %s --symbol-dce --reussir-compile-polymorphic-ffi --reussir-lowering-basic-ops --convert-to-llvm --reconcile-unrealized-casts | %reussir-translate --reussir-to-llvmir | %llc -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs -o %t.exe
// RUN: %t.exe

module attributes {reussir.drop_order = "forward", reussir.ffi_symbols = [@callback, @invoke]} {
  // CHECK: substitutions({target = @callback})
  reussir.polyffi texture("unsafe extern \"C\" { #[link_name = \"[:target:]\"] fn target(p: *mut u8) -> i64; } #[unsafe(no_mangle)] pub unsafe extern \"C\" fn invoke(p: *mut u8) -> i64 { unsafe { target(p) } }") substitutions({target = @callback})
  // CHECK: func.func private @callback
  func.func private @callback(%p: !llvm.ptr) -> i64 attributes {llvm.linkage = #llvm.linkage<linkonce_odr>} {
    %answer = arith.constant 42 : i64
    return %answer : i64
  }
  func.func private @invoke(!llvm.ptr) -> i64
  func.func @main() -> i32 {
    %null = llvm.mlir.zero : !llvm.ptr
    %answer = func.call @invoke(%null) : (!llvm.ptr) -> i64
    %expected = arith.constant 42 : i64
    %ok = arith.cmpi eq, %answer, %expected : i64
    %zero = arith.constant 0 : i32
    %one = arith.constant 1 : i32
    %result = arith.select %ok, %zero, %one : i32
    return %result : i32
  }
}

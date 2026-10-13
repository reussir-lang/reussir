// RUN: %reussir-opt %s --split-input-file --reussir-convert-to-llvm | %FileCheck %s

// `func.call` lowering looks its callee up (for `llvm.bareptr`) in a symbol
// table collection that the function conversion keeps up to date, instead
// of scanning the module once per call (quadratic in a module with many
// functions and calls). Callees defined before or after the call, declared,
// recursive, and already converted all resolve: the bare-pointer callee
// still gets one pointer for its memref, the others the full descriptor.

module {
  func.func private @early_bare(memref<4xi64>) attributes {llvm.bareptr}
  // CHECK-LABEL: llvm.func @user(
  // CHECK: llvm.call @early_bare(%{{.*}}) : (!llvm.ptr) -> ()
  // CHECK: llvm.call @later(%{{.*}}) : (i64) -> i64
  // CHECK: llvm.call @decl(%{{.*}}) : (i64) -> i64
  // CHECK: llvm.call @bare(%{{.*}}) : (!llvm.ptr) -> ()
  // CHECK: llvm.call @wide(%{{.*}}, %{{.*}}, %{{.*}}, %{{.*}}, %{{.*}}) : (!llvm.ptr, !llvm.ptr, i64, i64, i64) -> ()
  // CHECK: llvm.call @user(
  func.func @user(%x: i64, %m: memref<4xi64>) -> i64 {
    func.call @early_bare(%m) : (memref<4xi64>) -> ()
    %a = func.call @later(%x) : (i64) -> i64
    %b = func.call @decl(%a) : (i64) -> i64
    func.call @bare(%m) : (memref<4xi64>) -> ()
    func.call @wide(%m) : (memref<4xi64>) -> ()
    %c = func.call @user(%b, %m) : (i64, memref<4xi64>) -> i64
    return %c : i64
  }
  func.func private @decl(i64) -> i64
  func.func @later(%x: i64) -> i64 { return %x : i64 }
  func.func private @bare(memref<4xi64>) attributes {llvm.bareptr}
  // CHECK-LABEL: llvm.func @again(
  // CHECK: llvm.call @bare(%{{.*}}) : (!llvm.ptr) -> ()
  func.func @again(%m: memref<4xi64>) {
    func.call @bare(%m) : (memref<4xi64>) -> ()
    return
  }
  func.func private @wide(memref<4xi64>)

  // Reussir must still contribute type conversions when it has no operations.
  // CHECK-LABEL: llvm.func @rc_user(
  // CHECK-SAME: %{{.*}}: !llvm.ptr
  // CHECK: llvm.call @rc_decl(%{{.*}}) : (!llvm.ptr) -> !llvm.ptr
  func.func @rc_user(%p: !reussir.rc<f64>) -> !reussir.rc<f64> {
    %r = func.call @rc_decl(%p) : (!reussir.rc<f64>) -> !reussir.rc<f64>
    return %r : !reussir.rc<f64>
  }
  func.func private @rc_decl(!reussir.rc<f64>) -> !reussir.rc<f64>
}

// -----

// Cached callee lookup must retain the converter's target index width.
module @wasm32 attributes {llvm.data_layout = "e-p:32:32-i64:64-n32:64"} {
  // CHECK-LABEL: module @wasm32
  // CHECK-LABEL: llvm.func @user32(
  // CHECK: llvm.call @bare32(%{{.*}}) : (!llvm.ptr) -> ()
  // CHECK: llvm.call @wide32(%{{.*}}, %{{.*}}, %{{.*}}, %{{.*}}, %{{.*}}) : (!llvm.ptr, !llvm.ptr, i32, i32, i32) -> ()
  func.func @user32(%m: memref<4xi64>) {
    func.call @bare32(%m) : (memref<4xi64>) -> ()
    func.call @wide32(%m) : (memref<4xi64>) -> ()
    return
  }
  func.func private @bare32(memref<4xi64>) attributes {llvm.bareptr}
  func.func private @wide32(memref<4xi64>)
}

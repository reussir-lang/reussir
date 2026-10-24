// REQUIRES: openxla
// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

llvm.mlir.global external @mutable() : !llvm.struct<(array<64 x i8>, array<4 x i8>)>
func.func @mutable_callee() {
  // expected-error @+1 {{lowered callee must be a constant {checksum, bytecode} global}}
  %done = reussir.pjrt.jit_call @mutable() on devices [0] : () -> !ifrt.control
  return
}

// -----
llvm.mlir.global external constant @short_checksum() : !llvm.struct<(array<63 x i8>, array<4 x i8>)>
func.func @short_checksum_callee() {
  // expected-error @+1 {{lowered callee must be a constant {checksum, bytecode} global}}
  %done = reussir.pjrt.jit_call @short_checksum() on devices [0] : () -> !ifrt.control
  return
}

// -----
llvm.mlir.global external constant @wrong_payload() : !llvm.struct<(array<64 x i8>, array<4 x i32>)>
func.func @wrong_payload_callee() {
  // expected-error @+1 {{lowered callee must be a constant {checksum, bytecode} global}}
  %done = reussir.pjrt.jit_call @wrong_payload() on devices [0] : () -> !ifrt.control
  return
}

// -----
llvm.mlir.global external constant @non_struct() : i64
func.func @non_struct_callee() {
  // expected-error @+1 {{lowered callee must be a constant {checksum, bytecode} global}}
  %done = reussir.pjrt.jit_call @non_struct() on devices [0] : () -> !ifrt.control
  return
}

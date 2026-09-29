// RUN: %reussir-opt %s --emit-bytecode -o %t.bc
// RUN: %reussir-opt %t.bc | %FileCheck %s --check-prefix=PARSE
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --reussir-token-reuse | %FileCheck %s --check-prefix=TOKEN
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --reussir-rc-decrement-expansion --reussir-acquire-drop-expansion --reussir-convert-to-std --reussir-token-reuse --reussir-convert-to-std --convert-scf-to-cf --reussir-lowering-basic-ops --reussir-convert-to-llvm --reconcile-unrealized-casts --canonicalize -o %t.mlir
// RUN: %FileCheck %s --check-prefix=LLVM < %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir -o /dev/null

!D = !reussir.rc<!reussir.array<? x 3 x f32, #reussir.target<devices = [0]>>>
!V = memref<?x3xf32, strided<[?, ?], offset: ?>>

// PARSE-LABEL: func.func @upload
// PARSE: reussir.array.to_device
// TOKEN-LABEL: func.func @upload
// TOKEN: reussir.token.alloc : <align : 8, size : 32>
// TOKEN: reussir.array.to_device {{.*}} token(
// LLVM-LABEL: llvm.func @upload
// LLVM: llvm.intr.smul.with.overflow
// LLVM: llvm.call @__reussir_pjrt_array_from_host
// LLVM-NOT: llvm.call @__reussir_allocate
// LLVM: llvm.return
func.func @upload(%src: !V) -> !D {
  %d = reussir.array.to_device %src : !V -> !D
  return %d : !D
}

// PARSE-LABEL: func.func @download
// PARSE: reussir.array.to_host
// LLVM-LABEL: llvm.func @download
// LLVM: llvm.intr.umul.with.overflow
// LLVM: llvm.call @__reussir_pjrt_array_host_size
// LLVM: llvm.call @__reussir_pjrt_array_to_host
func.func @download(%d: !D, %dst: !V) {
  reussir.array.to_host %d into %dst : !D, !V
  return
}

// A dead host allocation can supply the descriptor/header, not device payload.
// TOKEN-LABEL: func.func @reuse_header
// TOKEN: reussir.rc.dec
// TOKEN: reussir.token.ensure
// TOKEN-NOT: reussir.token.alloc
// TOKEN: reussir.array.to_device {{.*}} token(
func.func @reuse_header(%old: !reussir.rc<!reussir.array<3 x i64>>, %src: !V) -> !D {
  reussir.rc.dec(%old : !reussir.rc<!reussir.array<3 x i64>>)
  %d = reussir.array.to_device %src : !V -> !D
  return %d : !D
}

// TOKEN-LABEL: func.func @reuse_device_header
// TOKEN: reussir.rc.dec
// TOKEN: reussir.token.ensure
// TOKEN-NOT: reussir.token.alloc
// TOKEN: reussir.array.to_device {{.*}} token(
func.func @reuse_device_header(%old: !D, %src: !V) -> !D {
  reussir.rc.dec(%old : !D)
  %d = reussir.array.to_device %src : !V -> !D
  return %d : !D
}

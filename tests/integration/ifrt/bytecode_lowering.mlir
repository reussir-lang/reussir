// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-convert-to-llvm | %FileCheck %s --implicit-check-not=reussir.ifrt.bytecode

module {
  // The call retains its symbol reference when the payload becomes a global.
  // CHECK-LABEL: llvm.func @host
  // CHECK: reussir.pjrt.jit_call @[[CODE:[a-zA-Z0-9_]+]]()
  func.func @host() attributes {ifrt.function} {
    %done = ifrt.Call @kernel::@main() on devices [0] : () -> ()
    return
  }
  // CHECK: llvm.mlir.global internal constant @[[CODE]]() {{.*}} : !llvm.struct<(array<64 x i8>, array<{{[0-9]+}} x i8>)>
  // CHECK: %[[BYTES:.*]] = llvm.mlir.constant(dense<{{.*}}> : tensor<{{[0-9]+}}xi8>)
  // CHECK: %[[HASH:.*]] = llvm.mlir.constant(dense<{{.*}}> : tensor<64xi8>)
  // CHECK: %[[FIRST:.*]] = llvm.insertvalue %[[HASH]], {{.*}}[0]
  // CHECK: %[[OBJECT:.*]] = llvm.insertvalue %[[BYTES]], %[[FIRST]][1]
  // CHECK: llvm.return %[[OBJECT]]
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main() {
      return
    }
  }
}

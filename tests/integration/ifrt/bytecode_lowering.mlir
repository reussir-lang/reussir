// REQUIRES: openxla
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --symbol-dce --reussir-convert-to-llvm | %FileCheck %s --implicit-check-not=reussir.ifrt.bytecode

module {
  // Runtime compilation references the lowered bytecode object.
  // CHECK-LABEL: llvm.func @host
  // CHECK: llvm.mlir.addressof @[[CODE:[a-zA-Z0-9_]+]]
  // CHECK: %[[BYTES:.*]] = llvm.getelementptr {{.*}}[0, 1, 0]
  // CHECK: %[[SIZE:.*]] = llvm.mlir.constant({{[0-9]+}} : i64)
  // CHECK: %[[CHECKSUM:.*]] = llvm.getelementptr {{.*}}[0, 0, 0]
  // CHECK: llvm.insertvalue %[[BYTES]], {{.*}}[0]
  // CHECK: llvm.insertvalue %[[SIZE]], {{.*}}[1]
  // CHECK: llvm.insertvalue %[[CHECKSUM]], {{.*}}[2]
  // CHECK: llvm.store
  // CHECK: llvm.call @__reussir_pjrt_launch
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

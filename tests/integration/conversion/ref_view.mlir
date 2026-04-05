// RUN: %reussir-opt %s --convert-to-llvm | %FileCheck %s
module @test {
  func.func @ref_to_memref(%ref : !reussir.ref<i64>) -> i64 {
    %view = reussir.ref.to_memref (%ref : !reussir.ref<i64>) : memref<i64>
    %value = memref.load %view[] : memref<i64>
    return %value : i64
  }
  // CHECK-LABEL: llvm.func @ref_to_memref(%arg0: !llvm.ptr) -> i64
  // CHECK: %[[DESC:.*]] = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64)>
  // CHECK: %[[DESC1:.*]] = llvm.insertvalue %arg0, %[[DESC]][0] : !llvm.struct<(ptr, ptr, i64)>
  // CHECK: %[[DESC2:.*]] = llvm.insertvalue %arg0, %[[DESC1]][1] : !llvm.struct<(ptr, ptr, i64)>
  // CHECK: %[[ZERO:.*]] = llvm.mlir.constant(0 : index) : i64
  // CHECK: %[[DESC3:.*]] = llvm.insertvalue %[[ZERO]], %[[DESC2]][2] : !llvm.struct<(ptr, ptr, i64)>
  // CHECK: %[[PTR:.*]] = llvm.extractvalue %[[DESC3]][1] : !llvm.struct<(ptr, ptr, i64)>
  // CHECK: %[[VALUE:.*]] = llvm.load %[[PTR]] : !llvm.ptr -> i64
  // CHECK: llvm.return %[[VALUE]] : i64
  // CHECK: }

  func.func @from_offset_memref(%view : memref<4xi64>) -> i64 {
    %subview = memref.subview %view[2] [1] [1]
      : memref<4xi64> to memref<i64, strided<[], offset: 2>>
    %ref = reussir.ref.from_memref (%subview : memref<i64, strided<[], offset: 2>>) : !reussir.ref<i64>
    %value = reussir.ref.load (%ref : !reussir.ref<i64>) : i64
    return %value : i64
  }
  // CHECK-LABEL: llvm.func @from_offset_memref(
  // CHECK: %[[BASE:.*]] = llvm.extractvalue %{{.*}}[1] : !llvm.struct<(ptr, ptr, i64)>
  // CHECK: %[[OFFSET:.*]] = llvm.mlir.constant(2 : index) : i64
  // CHECK: %[[PTR:.*]] = llvm.getelementptr %[[BASE]][%[[OFFSET]]] : (!llvm.ptr, i64) -> !llvm.ptr, i64
  // CHECK: %[[VALUE:.*]] = llvm.load %[[PTR]] : !llvm.ptr -> i64
  // CHECK: llvm.return %[[VALUE]] : i64
  // CHECK: }

  func.func @from_memref_to_ref(%view : memref<i64>) -> i64 {
    %ref = reussir.ref.from_memref (%view : memref<i64>) : !reussir.ref<i64>
    %value = reussir.ref.load (%ref : !reussir.ref<i64>) : i64
    return %value : i64
  }
  // CHECK-LABEL: llvm.func @from_memref_to_ref(
  // CHECK: %[[PTR:.*]] = llvm.extractvalue %{{.*}}[1] : !llvm.struct<(ptr, ptr, i64)>
  // CHECK: %[[VALUE:.*]] = llvm.load %[[PTR]] : !llvm.ptr -> i64
  // CHECK: llvm.return %[[VALUE]] : i64
  // CHECK: }
}

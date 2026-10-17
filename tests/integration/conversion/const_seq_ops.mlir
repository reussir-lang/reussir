// RUN: %reussir-opt %s --reussir-convert-to-std --convert-to-llvm | %FileCheck %s

module {
  // CHECK: llvm.mlir.global internal constant @seq(dense<[3, 5, 8]> : tensor<3xi32>)
  // CHECK-SAME: !llvm.array<3 x i32>
  reussir.const_seq.global @seq = dense<[3, 5, 8]> : tensor<3xi32>

  // CHECK-LABEL: @len
  func.func @len() -> index {
    // CHECK: %[[PTR:.*]] = llvm.mlir.addressof @seq : !llvm.ptr
    // CHECK: %[[LEN:.*]] = llvm.mlir.constant(3 : i64) : i64
    // CHECK: %[[UNDEF:.*]] = llvm.mlir.undef : !llvm.struct<(ptr, i64)>
    // CHECK: %[[WITH_PTR:.*]] = llvm.insertvalue %[[PTR]], %[[UNDEF]][0]
    // CHECK: %[[VIEW:.*]] = llvm.insertvalue %[[LEN]], %[[WITH_PTR]][1]
    // CHECK: llvm.extractvalue %[[VIEW]][1] : !llvm.struct<(ptr, i64)>
    %seq = reussir.const_seq.literal @seq : !reussir.const_seq<i32>
    %len = reussir.const_seq.len (%seq : !reussir.const_seq<i32>) : index
    return %len : index
  }

  // CHECK-LABEL: @get
  func.func @get(%seq: !reussir.const_seq<i32>, %i: index) -> i32 {
    // CHECK: %[[LEN:.*]] = llvm.extractvalue %{{.*}}[1] : !llvm.struct<(ptr, i64)>
    // CHECK: %[[COND:.*]] = llvm.icmp "ult" %{{.*}}, %[[LEN]] : i64
    // CHECK: scf.if %[[COND]] -> (i32) {
    // CHECK:   %[[DATA:.*]] = llvm.extractvalue %{{.*}}[0] : !llvm.struct<(ptr, i64)>
    // CHECK:   %[[GEP:.*]] = llvm.getelementptr %[[DATA]][%{{.*}}] : (!llvm.ptr, i64) -> !llvm.ptr, i32
    // CHECK:   %[[VAL:.*]] = llvm.load %[[GEP]] : !llvm.ptr -> i32
    // CHECK:   scf.yield %[[VAL]] : i32
    // CHECK: } else {
    // CHECK:   llvm.call @__reussir_panic
    // CHECK:   scf.yield %{{.*}} : i32
    // CHECK: }
    %x = reussir.const_seq.get (%seq : !reussir.const_seq<i32>) [%i] : i32
    return %x : i32
  }
}

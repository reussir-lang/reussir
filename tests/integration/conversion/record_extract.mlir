// RUN: %reussir-opt %s --convert-to-llvm | \
// RUN: %reussir-translate --mlir-to-llvmir | %opt -S -O3 | %FileCheck %s

!pair = !reussir.record<compound "Pair" {i32, i64}>

module {
  func.func @test_extract(%pair : !pair) -> i64 {
    %0 = reussir.record.extract(%pair : !pair)[1] : i64
    return %0 : i64
  }
}

// CHECK-LABEL: define i64 @test_extract(%Pair %0)
// CHECK: %[[val:[0-9a-z\.]+]] = extractvalue %Pair %0, 1
// CHECK: ret i64 %[[val]]

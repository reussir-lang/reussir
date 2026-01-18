// RUN: %reussir-opt %s --reussir-lowering-basic-ops | \
// RUN: %reussir-translate --mlir-to-llvmir | %FileCheck %s

!pair = !reussir.record<compound "Pair" {i32, i64}>

module {
  func.func @test_extract(%pair : !pair) -> i64 {
    %0 = reussir.record.extract(%pair : !pair)[1] : i64
    return %0 : i64
  }
}

// CHECK-LABEL: define i64 @test_extract(%Pair %0)
// CHECK: %[[val:[0-9]+]] = extractvalue %Pair %0, 1
// CHECK: ret i64 %[[val]]

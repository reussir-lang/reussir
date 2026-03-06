// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s

!pair = !reussir.record<compound "Pair" [value] { i32, i64 }>
!none = !reussir.record<compound "None" [value] {}>
!sum = !reussir.record<variant "Sum" {!pair, !none}>
!num = !reussir.record<variant "Num" {i64, !none}>

module {
  func.func @roundtrip(%a: i32, %b: i64, %n: i64) {
    %0 = reussir.rc.create_compound(%a, %b : i32, i64) : !reussir.rc<!pair>
    %1 = "reussir.rc.create_variant"(%a, %b) <{operandSegmentSizes = array<i32: 0, 2, 0, 0>, tag = 0 : index}> : (i32, i64) -> !reussir.rc<!sum>
    %2 = "reussir.rc.create_variant"(%n) <{operandSegmentSizes = array<i32: 1, 0, 0, 0>, tag = 0 : index}> : (i64) -> !reussir.rc<!num>
    return
  }
}

// CHECK-LABEL: func.func @roundtrip
// CHECK: reussir.rc.create_compound
// CHECK: "reussir.rc.create_variant"(%{{.*}}, %{{.*}}) <{operandSegmentSizes = array<i32: 0, 2, 0, 0>, tag = 0 : index}>
// CHECK: "reussir.rc.create_variant"(%{{.*}}) <{operandSegmentSizes = array<i32: 1, 0, 0, 0>, tag = 0 : index}>

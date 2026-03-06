// RUN: %reussir-opt %s --reussir-rc-create-fusion | %FileCheck %s

!pair = !reussir.record<compound "Pair" [value] { i32, i64 }>
!none = !reussir.record<compound "None" [value] {}>
!sum = !reussir.record<variant "Sum" {!pair, !none}>
!num = !reussir.record<variant "Num" {i64, !none}>

module {
  func.func @fuse_compound(%a: i32, %b: i64) -> !reussir.rc<!pair> {
    %c = reussir.record.compound(%a, %b : i32, i64) : !pair
    %rc = reussir.rc.create value(%c : !pair) : !reussir.rc<!pair>
    return %rc : !reussir.rc<!pair>
  }

  func.func @fuse_variant_total(%a: i32, %b: i64) -> !reussir.rc<!sum> {
    %c = reussir.record.compound(%a, %b : i32, i64) : !pair
    %v = reussir.record.variant [0] (%c : !pair) : !sum
    %rc = reussir.rc.create value(%v : !sum) : !reussir.rc<!sum>
    return %rc : !reussir.rc<!sum>
  }

  func.func @fuse_variant_value(%a: i64) -> !reussir.rc<!num> {
    %v = reussir.record.variant [0] (%a : i64) : !num
    %rc = reussir.rc.create value(%v : !num) : !reussir.rc<!num>
    return %rc : !reussir.rc<!num>
  }

  func.func @keep_variant(%a: i32, %b: i64) -> !sum {
    %c = reussir.record.compound(%a, %b : i32, i64) : !pair
    %v = reussir.record.variant [0] (%c : !pair) : !sum
    %rc = reussir.rc.create value(%v : !sum) : !reussir.rc<!sum>
    return %v : !sum
  }
}

// CHECK-LABEL: func.func @fuse_compound
// CHECK-NOT: reussir.record.compound
// CHECK: reussir.rc.create_compound

// CHECK-LABEL: func.func @fuse_variant_total
// CHECK-NOT: reussir.record.compound
// CHECK-NOT: reussir.record.variant
// CHECK: "reussir.rc.create_variant"(%{{.*}}, %{{.*}}) <{operandSegmentSizes = array<i32: 0, 2, 0, 0>, tag = 0 : index}>

// CHECK-LABEL: func.func @fuse_variant_value
// CHECK-NOT: reussir.record.variant
// CHECK: "reussir.rc.create_variant"(%{{.*}}) <{operandSegmentSizes = array<i32: 1, 0, 0, 0>, tag = 0 : index}>

// CHECK-LABEL: func.func @keep_variant
// CHECK: reussir.record.compound
// CHECK: reussir.record.variant
// CHECK: "reussir.rc.create_variant"(%{{.*}}, %{{.*}}) <{operandSegmentSizes = array<i32: 0, 2, 0, 0>, tag = 0 : index}>

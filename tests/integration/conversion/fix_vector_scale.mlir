// RUN: %reussir-opt %s --reussir-fix-vector-scale | %FileCheck %s

// A target without scalable vectors takes a scale of one: the types become
// fixed and the operations that exist only for scalable vectors are replaced.

// CHECK-LABEL: func.func @types(
// CHECK-SAME: %[[A:.*]]: vector<2x4xf32>, %[[M:.*]]: vector<16xi1>) -> vector<2x4xf32>
// CHECK: arith.addf %[[A]], %[[A]] : vector<2x4xf32>
func.func @types(%a: vector<2x[4]xf32>, %m: vector<[16]xi1>) -> vector<2x[4]xf32> {
  %r = arith.addf %a, %a : vector<2x[4]xf32>
  return %r : vector<2x[4]xf32>
}

// CHECK-LABEL: func.func @lanes()
// CHECK: %[[ONE:.*]] = arith.constant 1 : index
// CHECK: arith.muli %[[ONE]], %{{.*}} : index
func.func @lanes() -> index {
  %v = vector.vscale
  %c = arith.constant 16 : index
  %n = arith.muli %v, %c : index
  return %n : index
}

// CHECK-LABEL: func.func @parts(
// CHECK: %[[PART:.*]] = vector.extract_strided_slice %{{.*}} {offsets = [8], sizes = [4], strides = [1]} : vector<16xi8> to vector<4xi8>
// CHECK: vector.insert_strided_slice %[[PART]], %{{.*}} {offsets = [4], strides = [1]} : vector<4xi8> into vector<16xi8>
func.func @parts(%a: vector<[16]xi8>) -> vector<[16]xi8> {
  %p = vector.scalable.extract %a[8] : vector<4xi8> from vector<[16]xi8>
  %r = vector.scalable.insert %p, %a[4] : vector<4xi8> into vector<[16]xi8>
  return %r : vector<[16]xi8>
}

// A constant keeps its lanes.
// CHECK-LABEL: func.func @constant()
// CHECK: arith.constant dense<2.000000e+00> : vector<4xf32>
func.func @constant() -> vector<[4]xf32> {
  %c = arith.constant dense<2.0> : vector<[4]xf32>
  return %c : vector<[4]xf32>
}

// A fixed vector is left alone.
// CHECK-LABEL: func.func @fixed(%{{.*}}: vector<8xi32>) -> vector<8xi32>
func.func @fixed(%a: vector<8xi32>) -> vector<8xi32> {
  return %a : vector<8xi32>
}

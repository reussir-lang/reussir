// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-fix-vector-scale{scale=1})' | %FileCheck %s
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-fix-vector-scale{scale=3})' | %FileCheck %s --check-prefix=THREE
// RUN: %reussir-opt %s --reussir-fix-vector-scale | %FileCheck %s --check-prefix=DEFAULT
// RUN: %not %reussir-opt %s --pass-pipeline='builtin.module(reussir-fix-vector-scale{scale=0})' 2>&1 | %FileCheck %s --check-prefix=ZERO

// A target without scalable vectors takes a fixed scale: the types become
// fixed, with their scalable extent multiplied by it, and the operations that
// exist only for scalable vectors are replaced.

// The scale is 4 when none is given.
// DEFAULT-LABEL: func.func @types(
// DEFAULT-SAME: vector<2x16xf32>, %{{.*}}: vector<64xi1>) -> vector<2x16xf32>

// ZERO: the scale of a scalable vector is positive

// CHECK-LABEL: func.func @types(
// CHECK-SAME: %[[A:.*]]: vector<2x4xf32>, %[[M:.*]]: vector<16xi1>) -> vector<2x4xf32>
// CHECK: arith.addf %[[A]], %[[A]] : vector<2x4xf32>
// THREE-LABEL: func.func @types(
// THREE-SAME: %[[A:.*]]: vector<2x12xf32>, %[[M:.*]]: vector<48xi1>) -> vector<2x12xf32>
// THREE: arith.addf %[[A]], %[[A]] : vector<2x12xf32>
func.func @types(%a: vector<2x[4]xf32>, %m: vector<[16]xi1>) -> vector<2x[4]xf32> {
  %r = arith.addf %a, %a : vector<2x[4]xf32>
  return %r : vector<2x[4]xf32>
}

// CHECK-LABEL: func.func @lanes()
// CHECK: %[[ONE:.*]] = arith.constant 1 : index
// CHECK: arith.muli %[[ONE]], %{{.*}} : index
// THREE-LABEL: func.func @lanes()
// THREE: %[[SCALE:.*]] = arith.constant 3 : index
// THREE: arith.muli %[[SCALE]], %{{.*}} : index
func.func @lanes() -> index {
  %v = vector.vscale
  %c = arith.constant 16 : index
  %n = arith.muli %v, %c : index
  return %n : index
}

// CHECK-LABEL: func.func @parts(
// CHECK: %[[PART:.*]] = vector.extract_strided_slice %{{.*}} {offsets = [8], sizes = [4], strides = [1]} : vector<16xi8> to vector<4xi8>
// CHECK: vector.insert_strided_slice %[[PART]], %{{.*}} {offsets = [4], strides = [1]} : vector<4xi8> into vector<16xi8>
// A fixed part keeps its position and length.
// THREE-LABEL: func.func @parts(
// THREE: %[[PART:.*]] = vector.extract_strided_slice %{{.*}} {offsets = [8], sizes = [4], strides = [1]} : vector<48xi8> to vector<4xi8>
// THREE: vector.insert_strided_slice %[[PART]], %{{.*}} {offsets = [4], strides = [1]} : vector<4xi8> into vector<48xi8>
func.func @parts(%a: vector<[16]xi8>) -> vector<[16]xi8> {
  %p = vector.scalable.extract %a[8] : vector<4xi8> from vector<[16]xi8>
  %r = vector.scalable.insert %p, %a[4] : vector<4xi8> into vector<[16]xi8>
  return %r : vector<[16]xi8>
}

// The position and the length of a scalable part count multiples of the
// scale.
// CHECK-LABEL: func.func @scalable_parts(
// CHECK: %[[PART:.*]] = vector.extract_strided_slice %{{.*}} {offsets = [8], sizes = [4], strides = [1]} : vector<16xi8> to vector<4xi8>
// CHECK: vector.insert_strided_slice %[[PART]], %{{.*}} {offsets = [4], strides = [1]} : vector<4xi8> into vector<16xi8>
// THREE-LABEL: func.func @scalable_parts(
// THREE: %[[PART:.*]] = vector.extract_strided_slice %{{.*}} {offsets = [24], sizes = [12], strides = [1]} : vector<48xi8> to vector<12xi8>
// THREE: vector.insert_strided_slice %[[PART]], %{{.*}} {offsets = [12], strides = [1]} : vector<12xi8> into vector<48xi8>
func.func @scalable_parts(%a: vector<[16]xi8>) -> vector<[16]xi8> {
  %p = vector.scalable.extract %a[8] : vector<[4]xi8> from vector<[16]xi8>
  %r = vector.scalable.insert %p, %a[4] : vector<[4]xi8> into vector<[16]xi8>
  return %r : vector<[16]xi8>
}

// A constant keeps its lanes.
// CHECK-LABEL: func.func @constant()
// CHECK: arith.constant dense<2.000000e+00> : vector<4xf32>
// THREE-LABEL: func.func @constant()
// THREE: arith.constant dense<2.000000e+00> : vector<12xf32>
func.func @constant() -> vector<[4]xf32> {
  %c = arith.constant dense<2.0> : vector<[4]xf32>
  return %c : vector<[4]xf32>
}

// A fixed vector is left alone.
// CHECK-LABEL: func.func @fixed(%{{.*}}: vector<8xi32>) -> vector<8xi32>
func.func @fixed(%a: vector<8xi32>) -> vector<8xi32> {
  return %a : vector<8xi32>
}

// The mask operations that also take fixed vectors keep their form.
// CHECK-LABEL: func.func @masks(
// CHECK: %[[M:.*]] = llvm.intr.get.active.lane.mask %{{.*}}, %{{.*}} : i64, i64 to vector<16xi1>
// CHECK: reussir.vector.cttz_elts %[[M]] : vector<16xi1> -> i64
func.func @masks(%n: i64) -> i64 {
  %zero = arith.constant 0 : i64
  %m = llvm.intr.get.active.lane.mask %zero, %n : i64, i64 to vector<[16]xi1>
  %first = reussir.vector.cttz_elts %m : vector<[16]xi1> -> i64
  return %first : i64
}

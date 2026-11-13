// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,convert-vector-to-llvm,reussir-convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir | %FileCheck %s
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,arm-sve-legalize-vector-storage,convert-vector-to-llvm{enable-arm-sve},reussir-convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir | %FileCheck %s

// CHECK-LABEL: define <8 x float> @fixed(
// CHECK-SAME: <8 x float> %[[A:.*]], float %[[X:.*]])
// CHECK: %[[SPLAT:.*]] = shufflevector <8 x float>
// CHECK: fadd <8 x float> %[[A]], %[[SPLAT]]
func.func @fixed(%a: vector<8xf32>, %x: f32) -> vector<8xf32> {
  %s = vector.broadcast %x : f32 to vector<8xf32>
  %r = arith.addf %a, %s : vector<8xf32>
  return %r : vector<8xf32>
}

// A vector of several dimensions is an array of one-dimensional vectors.
// CHECK-LABEL: define [2 x <4 x i32>] @rows(
// CHECK: add <4 x i32>
// CHECK: add <4 x i32>
func.func @rows(%a: vector<2x4xi32>, %b: vector<2x4xi32>) -> vector<2x4xi32> {
  %r = arith.addi %a, %b : vector<2x4xi32>
  return %r : vector<2x4xi32>
}

// CHECK-LABEL: define float @scalable(
// CHECK-SAME: <vscale x 4 x float> %[[A:.*]], <vscale x 4 x i1> %[[M:.*]])
// CHECK: %[[LIVE:.*]] = select <vscale x 4 x i1> %[[M]], <vscale x 4 x float> %[[A]], <vscale x 4 x float> zeroinitializer
// CHECK: call float @llvm.vector.reduce.fadd.nxv4f32(float {{.*}}, <vscale x 4 x float> %[[LIVE]])
func.func @scalable(%a: vector<[4]xf32>, %m: vector<[4]xi1>) -> f32 {
  %z = arith.constant dense<0.0> : vector<[4]xf32>
  %live = arith.select %m, %a, %z : vector<[4]xi1>, vector<[4]xf32>
  %r = vector.reduction <add>, %live : vector<[4]xf32> into f32
  return %r : f32
}

// The scalable dimension is the last one.
// CHECK-LABEL: define [2 x <vscale x 4 x float>] @scalable_rows(
// CHECK: fadd <vscale x 4 x float>
// CHECK: fadd <vscale x 4 x float>
func.func @scalable_rows(%a: vector<2x[4]xf32>, %b: vector<2x[4]xf32>) -> vector<2x[4]xf32> {
  %r = arith.addf %a, %b : vector<2x[4]xf32>
  return %r : vector<2x[4]xf32>
}

// CHECK-LABEL: define <8 x i1> @mask(
// CHECK: icmp {{.*}} <8 x i32>
func.func @mask(%n: index) -> vector<8xi1> {
  %m = vector.create_mask %n : vector<8xi1>
  return %m : vector<8xi1>
}

// CHECK-LABEL: define i64 @lanes()
// CHECK: call i64 @llvm.vscale.i64()
func.func @lanes() -> i64 {
  %v = vector.vscale
  %c = arith.constant 4 : index
  %n = arith.muli %v, %c : index
  %r = arith.index_cast %n : index to i64
  return %r : i64
}

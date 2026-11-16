// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s

// CHECK-LABEL: func.func @cttz_elts
// CHECK: reussir.vector.cttz_elts %{{.*}} : vector<16xi1> -> i64
// CHECK: reussir.vector.cttz_elts %{{.*}} : vector<[16]xi1> -> i32
func.func @cttz_elts(%fixed: vector<16xi1>, %scalable: vector<[16]xi1>) -> (i64, i32) {
  %a = reussir.vector.cttz_elts %fixed : vector<16xi1> -> i64
  %b = reussir.vector.cttz_elts %scalable : vector<[16]xi1> -> i32
  return %a, %b : i64, i32
}

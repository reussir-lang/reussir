// RUN: %reussir-opt %s --reussir-convert-vector-to-llvm | %FileCheck %s
// RUN: %reussir-opt %s --reussir-convert-vector-to-llvm | %FileCheck %s --check-prefix=ARGS
// RUN: %reussir-opt %S/vector_lowering.mlir --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-convert-vector-to-llvm,reussir-convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir | %FileCheck %S/vector_lowering.mlir

// Two pairs of arms without any vector that release different values in the
// same way. The block merging of MLIR's `convert-vector-to-llvm` (the
// `Aggressive` level of the greedy driver's region simplification) turns
// each pair into one block that takes the values as arguments; in a
// large function with many such arms the arguments multiply from one merge
// to the next. The lowering leaves the blocks as they are, and no block
// gets an argument.

// ARGS-NOT: ^bb{{[0-9]+}}(

func.func private @release(i64, i64)

// CHECK-LABEL: func.func @arms(
// CHECK: cf.cond_br %{{.+}}, ^[[LEFT:bb[0-9]+]], ^[[RIGHT:bb[0-9]+]]
// CHECK-NEXT: ^[[LEFT]]:
// CHECK-NEXT: call @release(%[[A:[^,]+]], %[[A]])
// CHECK: ^[[RIGHT]]:
// CHECK-NEXT: call @release(%[[B:[^,]+]], %[[C:[^)]+]])
// CHECK: cf.cond_br %{{.+}}, ^[[LEFT2:bb[0-9]+]], ^[[RIGHT2:bb[0-9]+]]
// CHECK-NEXT: ^[[LEFT2]]:
// CHECK-NEXT: call @release(%[[C]], %[[C]])
// CHECK: ^[[RIGHT2]]:
// CHECK-NEXT: call @release(%[[A]], %[[B]])
func.func @arms(%p: i1, %a: i64, %b: i64, %c: i64) {
  cf.cond_br %p, ^left, ^right
^left:
  call @release(%a, %a) : (i64, i64) -> ()
  cf.br ^next
^right:
  call @release(%b, %c) : (i64, i64) -> ()
  cf.br ^next
^next:
  cf.cond_br %p, ^left2, ^right2
^left2:
  call @release(%c, %c) : (i64, i64) -> ()
  cf.br ^exit
^right2:
  call @release(%a, %b) : (i64, i64) -> ()
  cf.br ^exit
^exit:
  return
}

// The vector operations are still lowered.
// CHECK-LABEL: func.func @splat(
// CHECK-NOT: vector.broadcast
// CHECK: llvm.shufflevector
func.func @splat(%x: f32) -> vector<4xf32> {
  %v = vector.broadcast %x : f32 to vector<4xf32>
  return %v : vector<4xf32>
}

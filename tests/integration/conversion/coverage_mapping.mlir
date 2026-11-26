// RUN: %reussir-opt %s --reussir-emit-coverage-mapping='compilation-dir=/work' \
// RUN:   | %FileCheck %s
// RUN: %reussir-opt %s --reussir-emit-coverage-mapping='compilation-dir=/work' \
// RUN:   --convert-to-llvm -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir -o %t.ll
// RUN: %FileCheck %s --check-prefix=IR < %t.ll
// RUN: %llc -filetype=obj %t.ll -o %t.o
// RUN: %llvm-readobj --sections %t.o | %FileCheck %s --check-prefix=OBJ

// The record of `f` encodes one file, no expressions and three regions in
// the order of their start: the function, the branch, and the `then` arm.
// The name hash and the record name are those that clang gives to `f`.

// CHECK-NOT: reussir.coverage.map
// CHECK: llvm.comdat @__llvm_global_comdat {
// CHECK-NEXT: llvm.comdat_selector @__covrec_CC914F75DD4CA18Fu any
// CHECK: llvm.mlir.global linkonce_odr hidden constant @__covrec_CC914F75DD4CA18Fu() comdat(@__llvm_global_comdat::@__covrec_CC914F75DD4CA18Fu)
// CHECK-SAME: alignment = 8 : i64, section = "__llvm_covfun"
// CHECK-SAME: !llvm.struct<packed (i64, i32, i64, i64, array<21 x i8>)>
// CHECK: llvm.mlir.global private constant @__llvm_coverage_mapping()
// CHECK-SAME: section = "__llvm_covmap"
// CHECK: llvm.mlir.global appending @llvm.used()
// CHECK-SAME: section = "llvm.metadata"

// IR: $__covrec_CC914F75DD4CA18Fu = comdat any
// IR: @__covrec_CC914F75DD4CA18Fu = linkonce_odr hidden constant <{ i64, i32, i64, i64, [21 x i8] }> <{ i64 -3706093650706652785, i32 21, i64 1234, i64 {{-?[0-9]+}}, [21 x i8] c"\01\01\00\03\01\01\01\08\02 \05\09\01\08\00\0D\05\01\05\02\06" }>, section "__llvm_covfun", comdat, align 8
// IR: @__llvm_coverage_mapping = private constant { { i32, i32, i32, i32 }, {{\[}}[[SIZE:[0-9]+]] x i8] } { { i32, i32, i32, i32 } { i32 0, i32 [[SIZE]], i32 0, i32 6 }, {{\[}}[[SIZE]] x i8] c"{{.*}}" }, section "__llvm_covmap", align 8
// IR: @llvm.used = appending global [2 x ptr] [ptr @__covrec_CC914F75DD4CA18Fu, ptr @__llvm_coverage_mapping], section "llvm.metadata"

// OBJ-DAG: Name: __llvm_covfun
// OBJ-DAG: Name: __llvm_covmap

module attributes {llvm.target_triple = "x86_64-unknown-linux-gnu"} {
  func.func @f(%x: i64) -> i64 attributes {reussir.coverage.map = {
      hash = 1234 : i64, name = "f", counters = 3 : i32,
      regions = [
        {counter = 0 : i32, span = loc("a.rr":1:1 to 9:2)},
        {counter = 1 : i32, span = loc("a.rr":3:5 to 5:6)},
        {true_counter = 1 : i32, false_counter = 2 : i32,
         span = loc("a.rr":2:8 to 2:13)}]}} {
    return %x : i64
  }
}

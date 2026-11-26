// RUN: %reussir-opt %s -split-input-file -verify-diagnostics \
// RUN:   --reussir-emit-coverage-mapping | %FileCheck %s

// Mach-O has no comdats. The records go after the old `llvm.used` entries.

// CHECK-LABEL: llvm.func @g
// CHECK: llvm.mlir.global appending @llvm.used()
// CHECK-SAME: !llvm.array<3 x ptr>
// CHECK: llvm.mlir.addressof @g
// CHECK: llvm.mlir.addressof @[[RECORD:__covrec_[0-9A-F]+u]]
// CHECK: llvm.mlir.addressof @__llvm_coverage_mapping
// CHECK-NOT: comdat
// CHECK: llvm.mlir.global linkonce_odr hidden constant @[[RECORD]]()
// CHECK-SAME: section = "__LLVM_COV,__llvm_covfun"
// CHECK-NOT: comdat
// CHECK: section = "__LLVM_COV,__llvm_covmap"

module attributes {llvm.target_triple = "arm64-apple-macosx14.0.0"} {
  llvm.func @g() attributes {reussir.coverage.map = {
      hash = 7 : i64, name = "g", counters = 1 : i32,
      regions = [{counter = 0 : i32, span = loc("b.rr":1:1 to 2:2)}]}} {
    llvm.return
  }
  llvm.mlir.global appending @llvm.used() {section = "llvm.metadata"} : !llvm.array<1 x ptr> {
    %0 = llvm.mlir.addressof @g : !llvm.ptr
    %1 = llvm.mlir.undef : !llvm.array<1 x ptr>
    %2 = llvm.insertvalue %0, %1[0] : !llvm.array<1 x ptr>
    llvm.return %2 : !llvm.array<1 x ptr>
  }
}

// -----

module {
  // expected-error @+1 {{invalid 'reussir.coverage.map': a counter is not less than 'counters'}}
  func.func @h() attributes {reussir.coverage.map = {
      hash = 7 : i64, name = "h", counters = 1 : i32,
      regions = [{counter = 1 : i32, span = loc("c.rr":1:1 to 2:2)}]}} {
    return
  }
}

// -----

// A gap region sets the high bit of its end column.

// CHECK-LABEL: func.func @gap
// CHECK: llvm.mlir.constant("\01\01\00\02\01\01\01\03\02\05\01\03\01\85\80\80\80\08")

module {
  func.func @gap() attributes {reussir.coverage.map = {
      hash = 7 : i64, name = "gap", counters = 2 : i32,
      regions = [{counter = 0 : i32, span = loc("d.rr":1:1 to 4:2)},
                 {counter = 1 : i32, kind = "gap", span = loc("d.rr":2:3 to 3:5)}]}} {
    return
  }
}

// -----

module {
  // expected-error @+1 {{invalid 'reussir.coverage.map': expected no 'kind', or the kind "gap"}}
  func.func @skipped() attributes {reussir.coverage.map = {
      hash = 7 : i64, name = "skipped", counters = 1 : i32,
      regions = [{counter = 0 : i32, kind = "skipped", span = loc("e.rr":1:1 to 2:2)}]}} {
    return
  }
}

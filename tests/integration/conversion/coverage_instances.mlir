// RUN: %reussir-opt %s --reussir-instrument-coverage --mlir-print-local-scope | %FileCheck %s
// RUN: %reussir-opt %s --reussir-instrument-coverage --reussir-emit-coverage-mapping \
// RUN:   | %FileCheck %s --check-prefix=RECORD

// Each instance of a generic function gets its own PGO name and record, and
// the same regions and hash. llvm-cov finds the records at one span and shows
// them as instantiations of one function.

// The record names come from the PGO names, so they are different.
// RECORD-COUNT-3: llvm.mlir.global linkonce_odr hidden constant @__covrec_{{[0-9A-F]+}}u()
// RECORD-NOT: llvm.mlir.global {{.*}}@__covrec_

module {
  // CHECK-LABEL: func.func @_RINvC4pick4pickxE(
  // CHECK-SAME: reussir.coverage.map = {counters = 3 : i32, hash = [[HASH:-?[0-9]+]] : i64, name = "_RINvC4pick4pickxE", regions = [[REGIONS:\[.*\]]]}
  // CHECK: llvm.mlir.addressof @__profn__RINvC4pick4pickxE
  func.func @_RINvC4pick4pickxE(%flag: i1, %a: i64, %b: i64) -> i64 attributes {llvm.linkage = #llvm.linkage<weak_odr>} {
    %r = scf.if %flag -> (i64) {
      scf.yield %a : i64 loc("pick.rr":2:15 to 2:16)
    } else {
      scf.yield %b : i64 loc("pick.rr":2:26 to 2:27)
    } loc("pick.rr":2:5 to 2:29)
    return %r : i64 loc("pick.rr":2:5 to 2:29)
  } loc("pick.rr":1:40 to 3:2)

  // CHECK-LABEL: func.func @_RINvC4pick4pickdE(
  // CHECK-SAME: reussir.coverage.map = {counters = 3 : i32, hash = [[HASH]] : i64, name = "_RINvC4pick4pickdE", regions = [[REGIONS]]}
  // CHECK: llvm.mlir.addressof @__profn__RINvC4pick4pickdE
  func.func @_RINvC4pick4pickdE(%flag: i1, %a: f64, %b: f64) -> f64 attributes {llvm.linkage = #llvm.linkage<weak_odr>} {
    %r = scf.if %flag -> (f64) {
      scf.yield %a : f64 loc("pick.rr":2:15 to 2:16)
    } else {
      scf.yield %b : f64 loc("pick.rr":2:26 to 2:27)
    } loc("pick.rr":2:5 to 2:29)
    return %r : f64 loc("pick.rr":2:5 to 2:29)
  } loc("pick.rr":1:40 to 3:2)

  // CHECK-LABEL: func.func @_RINvC4pick4pickbE(
  // CHECK-SAME: reussir.coverage.map = {counters = 3 : i32, hash = [[HASH]] : i64, name = "_RINvC4pick4pickbE", regions = [[REGIONS]]}
  // CHECK: llvm.mlir.addressof @__profn__RINvC4pick4pickbE
  func.func @_RINvC4pick4pickbE(%flag: i1, %a: i1, %b: i1) -> i1 attributes {llvm.linkage = #llvm.linkage<weak_odr>} {
    %r = scf.if %flag -> (i1) {
      scf.yield %a : i1 loc("pick.rr":2:15 to 2:16)
    } else {
      scf.yield %b : i1 loc("pick.rr":2:26 to 2:27)
    } loc("pick.rr":2:5 to 2:29)
    return %r : i1 loc("pick.rr":2:5 to 2:29)
  } loc("pick.rr":1:40 to 3:2)
}

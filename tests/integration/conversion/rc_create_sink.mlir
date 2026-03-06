// RUN: %reussir-opt %s --reussir-rc-create-sink | %FileCheck %s

module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>> } {
  func.func @sink(%cond: i1,
                  %existing: !reussir.token<align: 8, size: 16>,
                  %value: i64) -> !reussir.rc<i64> {
    %token = "scf.if"(%cond) ({
      scf.yield %existing : !reussir.token<align: 8, size: 16>
    }, {
      %fresh = reussir.token.alloc : !reussir.token<align: 8, size: 16>
      scf.yield %fresh : !reussir.token<align: 8, size: 16>
    }) {reussir.expanded_ensure} : (i1) -> !reussir.token<align: 8, size: 16>
    %rc = reussir.rc.create value(%value : i64) token(%token : !reussir.token<align: 8, size: 16>) : !reussir.rc<i64>
    return %rc : !reussir.rc<i64>
  }
}

// CHECK-LABEL: func.func @sink
// CHECK: %[[RC:.*]] = scf.if %{{.*}} -> (!reussir.rc<i64>) {
// CHECK: reussir.rc.create value(%{{.*}} : i64) token(%{{.*}} : !reussir.token<{{.*}}>) skip_rc : !reussir.rc<i64>
// CHECK: scf.yield %{{.*}} : !reussir.rc<i64>
// CHECK: } else {
// CHECK: reussir.rc.create value(%{{.*}} : i64) token(%{{.*}} : !reussir.token<{{.*}}>) : !reussir.rc<i64>
// CHECK: scf.yield %{{.*}} : !reussir.rc<i64>
// CHECK: return %[[RC]] : !reussir.rc<i64>

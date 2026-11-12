// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-rc-decrement-expansion --canonicalize --cse --reussir-inc-dec-cancellation | %FileCheck %s --check-prefix=EXPAND
// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-rc-decrement-expansion --reussir-acquire-drop-expansion --convert-scf-to-cf --convert-to-llvm | %reussir-translate --mlir-to-llvmir | %opt -O3 -S | %FileCheck %s --check-prefix=LLVM

!R = !reussir.rc<i64>
!A = !reussir.rc<i64 shared atomic>
module attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>>} {
  // EXPAND-LABEL: @manual_pair(
  // EXPAND: reussir.rc.inc{{.*}}{reussir.unsafe}
  // EXPAND: reussir.rc.fetch{{.*}}{reussir.unsafe}
  // EXPAND: reussir.rc.set{{.*}}{reussir.unsafe}
  // LLVM-LABEL: define {{.*}}@manual_pair(
  // LLVM: load volatile i32
  // LLVM: store volatile i32
  // LLVM: load volatile i32
  // LLVM: store volatile i32
  func.func @manual_pair(%x: !R) {
    reussir.rc.inc(%x : !R) {reussir.unsafe}
    reussir.rc.dec(%x : !R) {reussir.unsafe}
    return
  }
  // EXPAND-LABEL: @atomic_pair(
  // EXPAND: reussir.rc.inc{{.*}}{reussir.unsafe}
  // EXPAND: reussir.rc.fetch_sub{{.*}}{reussir.unsafe}
  // LLVM-LABEL: define {{.*}}@atomic_pair(
  // LLVM: atomicrmw volatile add
  // LLVM: atomicrmw volatile sub
  func.func @atomic_pair(%x: !A) {
    reussir.rc.inc(%x : !A) {reussir.unsafe}
    reussir.rc.dec(%x : !A) {reussir.unsafe}
    return
  }
}

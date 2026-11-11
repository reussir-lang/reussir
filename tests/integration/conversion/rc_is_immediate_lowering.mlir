// RUN: %reussir-opt %s --split-input-file --convert-to-llvm | %FileCheck %s

// `reussir.rc.is_immediate` is true when an rc value is the immediate of a
// nullary arm. It never loads. Under TBI it is one test of the top byte, for
// any number of nullary arms; under the immortal encoding it compares the
// pointer with the dummy box of each nullary arm.

!t_a = !reussir.record<compound "T::A" [value] { }>
!t_b = !reussir.record<compound "T::B" [value] { }>
!t_c = !reussir.record<compound "T::C" [value] { i64, i64 }>
!t = !reussir.record<variant "T" { !t_a, !t_b, !t_c }>

module attributes { reussir.special_ptr_tag = "tbi", dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>> } {
  // CHECK-LABEL: llvm.func @is_imm_tbi
  // CHECK: %[[RAW:.+]] = llvm.ptrtoint %arg0 : !llvm.ptr to i64
  // CHECK: %[[TOP:.+]] = llvm.lshr %[[RAW]], %{{.+}} : i64
  // CHECK: %[[IMM:.+]] = llvm.icmp "ne" %[[TOP]], %{{.+}} : i64
  // CHECK-NOT: llvm.icmp
  // CHECK-NOT: llvm.load
  // CHECK: llvm.return %[[IMM]] : i1
  func.func @is_imm_tbi(%rc: !reussir.rc<!t>) -> i1 {
    %imm = reussir.rc.is_immediate (%rc : !reussir.rc<!t>)
    return %imm : i1
  }
}

// -----

!t_a = !reussir.record<compound "T::A" [value] { }>
!t_b = !reussir.record<compound "T::B" [value] { }>
!t_c = !reussir.record<compound "T::C" [value] { i64, i64 }>
!t = !reussir.record<variant "T" { !t_a, !t_b, !t_c }>

module attributes { reussir.special_ptr_tag = "immortal", dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>> } {
  // CHECK-LABEL: llvm.func @is_imm_immortal
  // CHECK: %[[A:.+]] = llvm.icmp "eq" %arg0, %{{.+}} : !llvm.ptr
  // CHECK: %[[B:.+]] = llvm.icmp "eq" %arg0, %{{.+}} : !llvm.ptr
  // CHECK: %[[IMM:.+]] = llvm.or %[[A]], %[[B]] : i1
  // CHECK-NOT: llvm.load
  // CHECK: llvm.return %[[IMM]] : i1
  func.func @is_imm_immortal(%rc: !reussir.rc<!t>) -> i1 {
    %imm = reussir.rc.is_immediate (%rc : !reussir.rc<!t>)
    return %imm : i1
  }
}

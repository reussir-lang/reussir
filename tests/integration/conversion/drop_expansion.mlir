// RUN: %reussir-opt %s --reussir-drop-expansion | %FileCheck %s
!list_ = !reussir.record<variant "List" incomplete>
!list_nil = !reussir.record<compound "List::Nil" [value] { }>
!list_cons = !reussir.record<compound "List::Cons" [value] { i64, !list_ }>
!list = !reussir.record<variant "List" { !list_nil, !list_cons }>

module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>> }  {
  func.func @test_ref_drop(%arg0: !reussir.ref<!list>) {
    // CHECK: reussir.record.dispatch
    reussir.ref.drop (%arg0 : !reussir.ref<!list>)
    return
  }

  // CHECK: func.func @test_ref_drop_tagged(%[[ARG:arg[0-9]+]]: !reussir.ref
  // CHECK: %[[REG0:[0-9]+]] = reussir.record.coerce[1] (%[[ARG]]
  // CHECK: %[[REG1:[0-9]+]] = reussir.ref.project(%[[REG0]]
  // CHECK: %[[REG2:[0-9]+]] = reussir.ref.load(%[[REG1]]
  // CHECK: reussir.rc.dec(%[[REG2]]
  func.func @test_ref_drop_tagged(%arg0: !reussir.ref<!list>) {
    reussir.ref.drop (%arg0 : !reussir.ref<!list>) variant [1]
    return
  }
}

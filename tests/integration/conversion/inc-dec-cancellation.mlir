// RUN: %reussir-opt %s --reussir-rc-decrement-expansion --reussir-drop-expansion --reussir-drop-expansion='expand-decrement=1 outline-record=1' | %FileCheck %s



!list_ = !reussir.record<variant "List" incomplete>
!list_nil = !reussir.record<compound "List::Nil" { }>
!list_cons = !reussir.record<compound "List::Cons" { i64, [shared] !list_ }>
!list = !reussir.record<variant "List" { !list_nil, !list_cons }>

module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>> }  {
  func.func @take_tail(%arg0: !reussir.rc<!list>) -> !reussir.rc<!list> {
    %ref = reussir.rc.borrow (%arg0 : !reussir.rc<!list>) : !reussir.ref<!list shared>
    %res = reussir.record.dispatch (%ref : !reussir.ref<!list shared>) -> !reussir.rc<!list> {
      [0] -> {
        ^bb_nil(%nil : !reussir.ref<!list_nil shared>):
          reussir.scf.yield %arg0 : !reussir.rc<!list>
      }
      [1] -> {
        ^bb_cons(%cons : !reussir.ref<!list_cons shared>):
          %tail_ref = reussir.ref.project (%cons : !reussir.ref<!list_cons shared>) [1] : !reussir.ref<!reussir.rc<!list> shared>
          %tail = reussir.ref.load (%tail_ref : !reussir.ref<!reussir.rc<!list> shared>) : !reussir.rc<!list>
          reussir.rc.inc (%tail : !reussir.rc<!list>)
          %token = reussir.rc.dec (%arg0 : !reussir.rc<!list>) : !reussir.nullable<!reussir.token<align: 8, size: 32>>
          // We current don't free the token here
          reussir.scf.yield %tail : !reussir.rc<!list>
      }
    }
    return %res : !reussir.rc<!list>
  }
}

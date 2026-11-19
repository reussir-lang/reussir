// REQUIRES: rcgmp
// RUN: %reussir-opt %s \
// RUN:   --reussir-rc-create-fusion \
// RUN:   --reussir-trmc-recursion-analysis | %FileCheck %s

!list_ = !reussir.record<variant "List" incomplete>
!list_cons = !reussir.record<compound "List::Cons" [value] {i64, !list_}>
!list_nil = !reussir.record<compound "List::Nil" [value] {}>
!list = !reussir.record<variant "List" {!list_cons, !list_nil}>
!rc_list = !reussir.rc<!list>
!counted = !reussir.record<compound "Counted" {i64, !rcgmp.mpz}>
!rc_counted = !reussir.rc<!counted>

module {
  func.func private @counted_drop(!reussir.ref<!counted>)

  // The release of an integer only returns memory, so drop glue for a record
  // that holds one does not block the rewrite.
  // CHECK-LABEL: func.func @integer_release_after(
  // CHECK: call @integer_release_after.trmc(
  func.func @integer_release_after(%n: i64, %owned: !rc_counted) -> !rc_list {
    %zero = arith.constant 0 : i64
    %done = arith.cmpi eq, %n, %zero : i64
    %result = scf.if %done -> (!rc_list) {
      reussir.rc.dec(%owned : !rc_counted)
      %nil = reussir.record.compound : !list_nil
      %variant = reussir.record.variant[1] (%nil : !list_nil) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    } else {
      %one = arith.constant 1 : i64
      %next = arith.subi %n, %one : i64
      reussir.rc.inc(%owned : !rc_counted)
      %rest = func.call @integer_release_after(%next, %owned) : (i64, !rc_counted) -> !rc_list
      %count = reussir.rc.fetch(%owned : !rc_counted) : index
      %single = arith.constant 1 : index
      %unique = arith.cmpi eq, %count, %single : index
      scf.if %unique {
        %ref = reussir.rc.borrow(%owned : !rc_counted) : !reussir.ref<!counted>
        func.call @counted_drop(%ref) : (!reussir.ref<!counted>) -> ()
      } else {
        %less = arith.subi %count, %single : index
        reussir.rc.set(%owned : !rc_counted, %less : index)
      } {reussir.expanded_decrement}
      %cons = reussir.record.compound(%n, %rest : i64, !rc_list) : !list_cons
      %variant = reussir.record.variant[0] (%cons : !list_cons) : !list
      %list = reussir.rc.create value(%variant : !list) : !rc_list
      scf.yield %list : !rc_list
    }
    return %result : !rc_list
  }
}

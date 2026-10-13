// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true retcon-drops=true' --reussir-token-reuse --reussir-convert-to-std --canonicalize --convert-scf-to-cf --reussir-convert-to-llvm --reconcile-unrealized-casts --verify-each | %reussir-translate --reussir-to-llvmir -o %t.ll
// RUN: %reussir-llvm-opt %t.ll -passes='reussir-retcon-drop,verify' -o %t.lowered.ll
// RUN: %llc %t.lowered.ll -O0 -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/Inputs/drop_member_order.c -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe
// RUN: %reussir-llvm-opt %t.ll -passes='reussir-retcon-drop,default<O3>,verify' -o %t.opt.ll
// RUN: %llc %t.opt.ll -O3 -relocation-model=pic -filetype=obj -o %t.opt.o
// RUN: %cc %t.opt.o %S/Inputs/drop_member_order.c -o %t.opt.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.opt.exe
//
// Transitive drop glue preserves field order across recursive suspension.
// Managed suffixes run after the entire child, including suffixes inline in
// a [value] record or nullable. A prefix runs before the child. The driver
// observes foreign cleanup on 100,000-cell chains with a 256 KiB stack.
// This covers the tail-selection hazard from lean2rr issue 13-d without
// importing its pending stack's different, last-pushed-first member policy.

!handle = !reussir.rc<!reussir.ffi_object<"Observed", @drop_handle>>
!chain_ = !reussir.record<compound "OrderChain" incomplete>
!chain = !reussir.record<compound "OrderChain" {!handle, !reussir.nullable<!reussir.rc<!chain_>>, !handle}>
!value_ = !reussir.record<compound "ValueChain" incomplete>
!boxed = !reussir.record<compound "BoxedHandle" {!handle}>
!inline = !reussir.record<compound "InlineHandle" [value] {!boxed}>
!value = !reussir.record<compound "ValueChain" {!reussir.nullable<!reussir.rc<!value_>>, !inline}>
!nullable_ = !reussir.record<compound "NullableChain" incomplete>
!nullable = !reussir.record<compound "NullableChain" {!reussir.nullable<!reussir.rc<!nullable_>>, !reussir.nullable<!handle>}>

module attributes {llvm.data_layout = "e-p:64:64-i64:64-n8:16:32:64-S128"} {
  func.func private @make_handle(i64) -> !handle
  func.func private @drop_handle(!handle)
  func.func @order_cons(%next: !reussir.nullable<!reussir.rc<!chain>>, %id: i64) -> !reussir.rc<!chain> {
    %two = arith.constant 2 : i64
    %one = arith.constant 1 : i64
    %before_id = arith.muli %id, %two : i64
    %after_id = arith.addi %before_id, %one : i64
    %before = func.call @make_handle(%before_id) : (i64) -> !handle
    %after = func.call @make_handle(%after_id) : (i64) -> !handle
    %payload = reussir.record.compound(%before, %next, %after : !handle, !reussir.nullable<!reussir.rc<!chain>>, !handle) : !chain
    %result = reussir.rc.create value(%payload : !chain) : !reussir.rc<!chain>
    return %result : !reussir.rc<!chain>
  }
  func.func @order_consume(%chain: !reussir.rc<!chain>) -> i64 {
    reussir.rc.dec(%chain : !reussir.rc<!chain>)
    %zero = arith.constant 0 : i64
    return %zero : i64
  }
  func.func @order_value_cons(%next: !reussir.nullable<!reussir.rc<!value>>, %id: i64) -> !reussir.rc<!value> {
    %handle = func.call @make_handle(%id) : (i64) -> !handle
    %box_value = reussir.record.compound(%handle : !handle) : !boxed
    %box = reussir.rc.create value(%box_value : !boxed) : !reussir.rc<!boxed>
    %after = reussir.record.compound(%box : !reussir.rc<!boxed>) : !inline
    %payload = reussir.record.compound(%next, %after : !reussir.nullable<!reussir.rc<!value>>, !inline) : !value
    %result = reussir.rc.create value(%payload : !value) : !reussir.rc<!value>
    return %result : !reussir.rc<!value>
  }
  func.func @order_value_consume(%chain: !reussir.rc<!value>) -> i64 {
    reussir.rc.dec(%chain : !reussir.rc<!value>)
    %zero = arith.constant 0 : i64
    return %zero : i64
  }
  func.func @order_nullable_cons(%next: !reussir.nullable<!reussir.rc<!nullable>>, %id: i64) -> !reussir.rc<!nullable> {
    %handle = func.call @make_handle(%id) : (i64) -> !handle
    %after = reussir.nullable.create (%handle : !handle) : !reussir.nullable<!handle>
    %payload = reussir.record.compound(%next, %after : !reussir.nullable<!reussir.rc<!nullable>>, !reussir.nullable<!handle>) : !nullable
    %result = reussir.rc.create value(%payload : !nullable) : !reussir.rc<!nullable>
    return %result : !reussir.rc<!nullable>
  }
  func.func @order_nullable_consume(%chain: !reussir.rc<!nullable>) -> i64 {
    reussir.rc.dec(%chain : !reussir.rc<!nullable>)
    %zero = arith.constant 0 : i64
    return %zero : i64
  }
}

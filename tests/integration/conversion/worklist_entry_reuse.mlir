// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},func.func(reussir-token-reuse{reuse-across-call=false}))' --verify-each | %FileCheck %s --check-prefix=LOCAL
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},func.func(reussir-token-reuse{reuse-across-call=true}))' --verify-each | %FileCheck %s --check-prefix=ACROSS

!node_ = !reussir.record<compound "EntryReuse" incomplete>
!link = !reussir.nullable<!reussir.rc<!node_ shared>>
!node = !reussir.record<compound "EntryReuse" { i64, !link, !link }>
!leaf = !reussir.record<compound "ReuseLeaf" { i64 }>
!rc = !reussir.rc<!leaf>

// LOCAL-LABEL: func.func @reuse
// LOCAL: reussir.token.free
// LOCAL: call @_RNvC{{[0-9]+}}REUSSIR_DROP_entry_forward{{[A-Za-z0-9_]+}}
// LOCAL: reussir.token.alloc
// ACROSS-LABEL: func.func @reuse
// ACROSS-NOT: reussir.token.free
// ACROSS: call @_RNvC{{[0-9]+}}REUSSIR_DROP_entry_forward{{[A-Za-z0-9_]+}}
// ACROSS-NOT: reussir.token.alloc
// ACROSS: reussir.token.ensure
module {
  func.func @reuse(%old: !rc, %ref: !reussir.ref<!node>, %value: i64) -> !rc {
    reussir.rc.dec(%old : !rc)
    reussir.ref.drop(%ref : !reussir.ref<!node>)
    %leaf = reussir.record.compound(%value : i64) : !leaf
    %new = reussir.rc.create value(%leaf : !leaf) : !rc
    return %new : !rc
  }
}

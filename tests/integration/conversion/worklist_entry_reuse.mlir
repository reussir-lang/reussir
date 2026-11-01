// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},func.func(reussir-token-reuse{reuse-across-call=false}))' --verify-each | %FileCheck %s --check-prefix=LOCAL
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},func.func(reussir-token-reuse{reuse-across-call=true}))' --verify-each | %FileCheck %s --check-prefix=ACROSS

!node_ = !reussir.record<compound "EntryReuse" incomplete>
!link = !reussir.nullable<!reussir.rc<!node_ shared>>
!node = !reussir.record<compound "EntryReuse" { i64, !link, !link }>
!rc = !reussir.rc<i64>
!token = !reussir.token<align: 8, size: 16>
!nullable = !reussir.nullable<!token>

// LOCAL-LABEL: func.func @reuse
// LOCAL: reussir.token.free
// LOCAL: func.call @__reussir_drop.entry.forward
// LOCAL: reussir.token.alloc
// ACROSS-LABEL: func.func @reuse
// ACROSS-NOT: reussir.token.free
// ACROSS: func.call @__reussir_drop.entry.forward
// ACROSS-NOT: reussir.token.alloc
// ACROSS: reussir.token.ensure
module {
  func.func @reuse(%old: !rc, %ref: !reussir.ref<!node>, %value: i64) -> !rc {
    %donor = reussir.rc.dec(%old : !rc) : !nullable
    reussir.ref.drop(%ref : !reussir.ref<!node>)
    %token = reussir.token.alloc : !token
    %new = reussir.rc.create value(%value : i64) token(%token : !token) : !rc
    return %new : !rc
  }
}

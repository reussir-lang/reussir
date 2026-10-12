// RUN: %reussir-opt %s --split-input-file --reussir-acquire-drop-expansion='outline-record=true expand-decrement=true' --verify-each | %FileCheck %s
// RUN: %reussir-opt %s --split-input-file --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-acquire-drop-expansion{outline-record=true expand-decrement=true},func.func(reussir-token-reuse),reussir-convert-to-std,canonicalize,expand-strided-metadata,lower-affine,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' | %reussir-translate --mlir-to-llvmir > %t.ll

!node_ = !reussir.record<compound "AtomicNode" incomplete>
!node = !reussir.record<compound "AtomicNode" { !reussir.nullable<!reussir.rc<!node_ shared atomic>> }>
// CHECK-LABEL: func.func private @{{.*}}AtomicNodeE.tail_release
// CHECK: reussir.rc.fetch_sub
// CHECK-NOT: reussir.rc.set
// CHECK: reussir.token.free
// CHECK-NOT: reussir.rc.set
// CHECK-LABEL: func.func @atomic_release
module {
  func.func @atomic_release(%ref: !reussir.ref<!reussir.rc<!node shared atomic>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.rc<!node shared atomic>>)
    return
  }
}

// -----

!node_ = !reussir.record<compound "ArrayNode" incomplete>
!node = !reussir.record<compound "ArrayNode" { !reussir.array<1x!reussir.nullable<!reussir.rc<!node_ shared>>> }>
// CHECK-LABEL: func.func private @{{.*}}ArrayNodeE.tail_release
// CHECK-NOT: call
// CHECK: reussir.array.view
// CHECK: reussir.array.project
// CHECK: reussir.ref.load
// CHECK: reussir.token.free
// CHECK-NOT: call
// CHECK-LABEL: func.func @array_release
module {
  func.func @array_release(%ref: !reussir.ref<!reussir.rc<!node shared>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.rc<!node shared>>)
    return
  }
}

// -----

!node_ = !reussir.record<compound "CellNode" incomplete>
!node = !reussir.record<compound "CellNode" { !reussir.cell<!reussir.nullable<!reussir.rc<!node_ shared>>> }>
// CHECK-LABEL: func.func private @{{.*}}CellNodeE.tail_release
// CHECK-NOT: call
// CHECK: reussir.ref.project
// CHECK: reussir.ref.load
// CHECK: reussir.token.free
// CHECK-NOT: call
// CHECK-LABEL: func.func @cell_release
module {
  func.func @cell_release(%ref: !reussir.ref<!reussir.rc<!node shared>>) {
    reussir.ref.drop (%ref : !reussir.ref<!reussir.rc<!node shared>>)
    return
  }
}

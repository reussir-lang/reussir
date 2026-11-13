// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s

// A pointer owns nothing: acquiring or dropping one is a no-op, and a record
// holding pointers only needs no member-wise traversal.

!P = !ptr.ptr<#ptr.generic_space>
!pointers = !reussir.record<compound "Pointers" [value] { !P, !P }>
!mixed = !reussir.record<compound "Mixed" [value] { !P, !reussir.rc<i64 atomic> }>

module attributes {dlti.dl_spec = #dlti.dl_spec<!P = #ptr.spec<size = 64, abi = 64, preferred = 64>>} {
  // CHECK-LABEL: func.func @pointer
  // CHECK-NEXT: return
  func.func @pointer(%ref: !reussir.ref<!P>) {
    reussir.ref.acquire(%ref : !reussir.ref<!P>)
    reussir.ref.drop(%ref : !reussir.ref<!P>)
    return
  }

  // CHECK-LABEL: func.func @pointers
  // CHECK-NEXT: return
  func.func @pointers(%ref: !reussir.ref<!pointers>) {
    reussir.ref.acquire(%ref : !reussir.ref<!pointers>)
    reussir.ref.drop(%ref : !reussir.ref<!pointers>)
    return
  }

  // CHECK-LABEL: func.func @mixed
  // CHECK: reussir.ref.project(%arg0 : {{.*}}) [1]
  // CHECK: reussir.rc.inc
  // CHECK-NOT: reussir.rc.inc
  func.func @mixed(%ref: !reussir.ref<!mixed>) {
    reussir.ref.acquire(%ref : !reussir.ref<!mixed>)
    return
  }
}

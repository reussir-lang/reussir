// RUN: %reussir-opt %s --reussir-token-instantiation --convert-to-llvm | %reussir-translate --mlir-to-llvmir | %opt -S -passes=instcombine | %FileCheck %s
// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-rc-create-fusion --convert-to-llvm | %reussir-translate --mlir-to-llvmir | %opt -S -passes=instcombine | %FileCheck %s

!a = !reussir.record<compound "A" [value] {i64}>
!b = !reussir.record<compound "B" [value] {i64}>
!t = !reussir.record<variant "BothInhabited" {!a, !b}>
!rc = !reussir.rc<!t>
!token = !reussir.token<align: 8, size: 16>
module attributes {reussir.special_ptr_tag = "tbi", dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  // No nullary arm is needed to enable pointer tags.
  // CHECK-LABEL: define ptr @create_a(
  // CHECK: or{{( disjoint)?}} i64 {{.*}}, 72057594037927936
  // CHECK: inttoptr
  // CHECK: ret ptr
  func.func @create_a(%v: i64) -> !rc {
    %a = reussir.record.compound(%v : i64) : !a
    %t = reussir.record.variant [0] (%a : !a) : !t
    %rc = reussir.rc.create value(%t : !t) : !rc
    return %rc : !rc
  }
  // A reused allocation loses the old tag and gets the accepting arm's tag.
  // CHECK-LABEL: define ptr @reuse_as_b(
  // CHECK: call ptr @llvm.ptrmask.p0.i64(ptr %0, i64 72057594037927935)
  // CHECK: or{{( disjoint)?}} i64 {{.*}}, 144115188075855872
  // CHECK: inttoptr
  // CHECK: ret ptr
  func.func @reuse_as_b(%old: !rc, %v: i64) -> !rc {
    %token = reussir.rc.reinterpret (%old : !rc) : !token
    %b = reussir.record.compound(%v : i64) : !b
    %t = reussir.record.variant [1] (%b : !b) : !t
    %new = reussir.rc.create value(%t : !t) token(%token : !token) : !rc
    return %new : !rc
  }
  // CHECK-LABEL: define i64 @tag(
  // CHECK-NOT: load
  // CHECK-NOT: br
  // CHECK: lshr i64 {{.*}}, 56
  // CHECK: add nsw i64 {{.*}}, -1
  // CHECK: ret i64
  func.func @tag(%rc: !rc) -> index {
    %ref = reussir.rc.borrow (%rc : !rc) : !reussir.ref<!t>
    %tag = reussir.record.tag(%ref : !reussir.ref<!t>) : index
    return %tag : index
  }
  // CHECK-LABEL: define ptr @create_atomic(
  // CHECK: or i64 {{.*}}, 72057594037927936
  // CHECK: ret ptr
  func.func @create_atomic(%v: i64) -> !reussir.rc<!t atomic> {
    %a = reussir.record.compound(%v : i64) : !a
    %t = reussir.record.variant [0] (%a : !a) : !t
    %rc = reussir.rc.create value(%t : !t) : !reussir.rc<!t atomic>
    return %rc : !reussir.rc<!t atomic>
  }
  // CHECK-LABEL: define ptr @from_memref(
  // CHECK: call ptr @llvm.ptrmask
  // CHECK: load i32
  // CHECK: shl
  // CHECK: or
  // CHECK: ret ptr
  func.func @from_memref(%view: memref<!t>) -> !reussir.ref<!t> {
    %ref = reussir.ref.from_memref(%view : memref<!t>) : !reussir.ref<!t>
    return %ref : !reussir.ref<!t>
  }
  // CHECK-LABEL: define i64 @spilled_tag(
  // CHECK: lshr i64 {{.*}}, 56
  // CHECK: or i64 {{.*}}, 2
  // CHECK: ret i64
  func.func @spilled_tag(%v: i64) -> index {
    %b = reussir.record.compound(%v : i64) : !b
    %t = reussir.record.variant [1] (%b : !b) : !t
    %ref = reussir.ref.spilled(%t : !t) : !reussir.ref<!t>
    %tag = reussir.record.tag(%ref : !reussir.ref<!t>) : index
    return %tag : index
  }
  // CHECK-LABEL: define void @dispose(
  // CHECK: call ptr @llvm.ptrmask
  // CHECK: call void @__reussir_deallocate
  func.func @dispose(%rc: !rc) {
    %token = reussir.rc.reinterpret(%rc : !rc) : !token
    reussir.token.free(%token : !token)
    return
  }
}

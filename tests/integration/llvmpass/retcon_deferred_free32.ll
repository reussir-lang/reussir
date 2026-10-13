; RUN: %reussir-llvm-opt %s -passes='reussir-retcon-deferred-free,verify' -o - | %FileCheck %s
; Offsets and descriptor fields follow the target pointer size, not the host.
target datalayout = "e-p:32:32-i64:64-n8:16:32-S128"
; CHECK: @action = private constant { ptr, i32 } { ptr null, i32 4 }
@action = private constant {ptr, i32} {ptr @resume, i32 0}, !reussir.dtor.resume !0
!0 = !{}
declare void @__reussir_dealloc_unsized(ptr)
define private void @resume(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i32 4
  %object = load ptr, ptr %slot
  call void @__reussir_dealloc_unsized(ptr %object)
  store {ptr, ptr} zeroinitializer, ptr %out
  ret void
}

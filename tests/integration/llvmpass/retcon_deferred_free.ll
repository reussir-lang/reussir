; RUN: %reussir-llvm-opt %s -passes='reussir-retcon-deferred-free,verify' -o - | %FileCheck %s
; RUN: %reussir-llvm-opt %s -passes='reussir-retcon-deferred-free,reussir-retcon-deferred-free,verify' -o - | %FileCheck %s
;
; Generated callbacks may be replaced only when they unconditionally load one
; payload pointer from the saved frame, free it, and return null on completion. Keep
; ordering-sensitive/conditional work in callbacks. Repeat the pass to check
; descriptor finalization is idempotent.
target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
declare void @__reussir_dealloc_unsized(ptr)
declare void @observe(ptr)
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
!0 = !{}
; CHECK: @completion.action = private constant { ptr, i64 } { ptr null, i64 24 }
@completion.action = private constant {ptr, i64} {ptr @completion, i64 0}, !reussir.dtor.resume !0
; CHECK: @field_stores.action = private constant { ptr, i64 } { ptr @field_stores, i64 0 }
@field_stores.action = private constant {ptr, i64} {ptr @field_stores, i64 0}, !reussir.dtor.resume !0
; CHECK: @memset_zero.action = private constant { ptr, i64 } { ptr @memset_zero, i64 0 }
@memset_zero.action = private constant {ptr, i64} {ptr @memset_zero, i64 0}, !reussir.dtor.resume !0
; CHECK: @non_null_resume.action = private constant { ptr, i64 } { ptr @non_null_resume, i64 0 }
@non_null_resume.action = private constant {ptr, i64} {ptr @non_null_resume, i64 0}, !reussir.dtor.resume !0
; CHECK: @side_effect.action = private constant { ptr, i64 } { ptr @side_effect, i64 0 }
@side_effect.action = private constant {ptr, i64} {ptr @side_effect, i64 0}, !reussir.dtor.resume !0
; CHECK: @volatile_payload.action = private constant { ptr, i64 } { ptr @volatile_payload, i64 0 }
@volatile_payload.action = private constant {ptr, i64} {ptr @volatile_payload, i64 0}, !reussir.dtor.resume !0
; CHECK: @atomic_payload.action = private constant { ptr, i64 } { ptr @atomic_payload, i64 0 }
@atomic_payload.action = private constant {ptr, i64} {ptr @atomic_payload, i64 0}, !reussir.dtor.resume !0
; CHECK: @volatile_output.action = private constant { ptr, i64 } { ptr @volatile_output, i64 0 }
@volatile_output.action = private constant {ptr, i64} {ptr @volatile_output, i64 0}, !reussir.dtor.resume !0
; CHECK: @volatile_memset.action = private constant { ptr, i64 } { ptr @volatile_memset, i64 0 }
@volatile_memset.action = private constant {ptr, i64} {ptr @volatile_memset, i64 0}, !reussir.dtor.resume !0
; CHECK: @two_frees.action = private constant { ptr, i64 } { ptr @two_frees, i64 0 }
@two_frees.action = private constant {ptr, i64} {ptr @two_frees, i64 0}, !reussir.dtor.resume !0
; CHECK: @negative_offset.action = private constant { ptr, i64 } { ptr @negative_offset, i64 0 }
@negative_offset.action = private constant {ptr, i64} {ptr @negative_offset, i64 0}, !reussir.dtor.resume !0
; CHECK: @wrong_base.action = private constant { ptr, i64 } { ptr @wrong_base, i64 0 }
@wrong_base.action = private constant {ptr, i64} {ptr @wrong_base, i64 0}, !reussir.dtor.resume !0
; CHECK: @partial_completion.action = private constant { ptr, i64 } { ptr @partial_completion, i64 0 }
@partial_completion.action = private constant {ptr, i64} {ptr @partial_completion, i64 0}, !reussir.dtor.resume !0
; CHECK: @conditional.action = private constant { ptr, i64 } { ptr @conditional, i64 0 }
@conditional.action = private constant {ptr, i64} {ptr @conditional, i64 0}, !reussir.dtor.resume !0
; CHECK: @untagged.action = private constant { ptr, i64 } { ptr @untagged, i64 0 }
@untagged.action = private constant {ptr, i64} {ptr @untagged, i64 0}

define private ptr @completion(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  ret ptr null
}

define private ptr @field_stores(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  store ptr null, ptr %out
  %child = getelementptr {ptr, ptr}, ptr %out, i32 0, i32 1
  store ptr null, ptr %child
  ret ptr null
}

define private ptr @memset_zero(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  call void @llvm.memset.p0.i64(ptr %out, i8 0, i64 16, i1 false)
  ret ptr null
}

define private ptr @non_null_resume(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  ret ptr @non_null_resume.action
}

define private ptr @side_effect(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  call void @observe(ptr %frame)
  ret ptr null
}

define private ptr @volatile_payload(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load volatile ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  ret ptr null
}

define private ptr @atomic_payload(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load atomic ptr, ptr %slot monotonic, align 8
  call void @__reussir_dealloc_unsized(ptr %object)
  ret ptr null
}

define private ptr @volatile_output(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  store volatile ptr null, ptr %out
  ret ptr null
}

define private ptr @volatile_memset(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  call void @llvm.memset.p0.i64(ptr %out, i8 0, i64 16, i1 true)
  ret ptr null
}

define private ptr @two_frees(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  call void @__reussir_dealloc_unsized(ptr %object)
  ret ptr null
}

define private ptr @negative_offset(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 -8
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  ret ptr null
}

define private ptr @wrong_base(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %out, i64 24
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  ret ptr null
}

define private ptr @partial_completion(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  store i8 0, ptr %out
  ret ptr null
}

define private ptr @conditional(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load ptr, ptr %slot, align 1
  br i1 %cancel, label %done, label %free
free:
  call void @__reussir_dealloc_unsized(ptr %object)
  br label %done
done:
  ret ptr null
}

define private ptr @untagged(ptr %frame, i1 %cancel, ptr %out) {
  %slot = getelementptr i8, ptr %frame, i64 24
  %object = load ptr, ptr %slot, align 1
  call void @__reussir_dealloc_unsized(ptr %object)
  ret ptr null
}

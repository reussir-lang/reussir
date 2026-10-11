// A member linking to an opaque foreign object is spelled as an explicit
// shared rc: an `ffi_object` has no capability to derive the link from and
// no inline layout. The frontend emits it for every record with an
// `#[ffi]` member, so the printed form must parse back.
// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s
!vec = !reussir.rc<!reussir.ffi_object<"::reussir_rt::collections::vec::Vec<u64>", @vec_drop>>
!holder = !reussir.record<compound "holder" {!vec, i64}>
!payload = !reussir.record<compound "opt.some" [value] {!vec}>
!atomic_vec = !reussir.rc<!reussir.ffi_object<"::reussir_rt::collections::vec::Vec<u64>", @vec_drop> atomic>
!regional = !reussir.record<compound "regional" [regional] {!vec}>
!atomic_holder = !reussir.record<compound "atomic_holder" {!atomic_vec}>
!atomic_payload = !reussir.record<compound "atomic_payload" [value] {!atomic_vec}>
!atomic_regional = !reussir.record<compound "atomic_regional" [regional] {!atomic_vec}>
!inner = !reussir.record<compound "inner" {i64}>
!atomic_record = !reussir.record<compound "atomic_record" {!reussir.rc<!inner atomic>}>
module @test {
  func.func private @vec_drop(!vec)
  // CHECK: func.func private @holder() -> !reussir.record<compound "holder" {!reussir.rc<!reussir.ffi_object<"::reussir_rt::collections::vec::Vec<u64>", @vec_drop>>, i64}>
  func.func private @holder() -> !holder
  // CHECK: func.func private @payload() -> !reussir.record<compound "opt.some" [value] {!reussir.rc<!reussir.ffi_object<"::reussir_rt::collections::vec::Vec<u64>", @vec_drop>>}>
  func.func private @payload() -> !payload
  // Shared FFI links work in regional containers too; normal and atomic
  // links round-trip in all three container capabilities.
  // CHECK-LABEL: func.func private @regional()
  // CHECK-SAME: [regional] {!reussir.rc<!reussir.ffi_object<
  func.func private @regional() -> !regional
  // CHECK-LABEL: func.func private @atomic_holder()
  // CHECK-SAME: @vec_drop> atomic>}
  func.func private @atomic_holder() -> !atomic_holder
  // CHECK-LABEL: func.func private @atomic_payload()
  // CHECK-SAME: [value] {{.*}}@vec_drop> atomic>}
  func.func private @atomic_payload() -> !atomic_payload
  // CHECK-LABEL: func.func private @atomic_regional()
  // CHECK-SAME: [regional] {{.*}}@vec_drop> atomic>}
  func.func private @atomic_regional() -> !atomic_regional
  // Ordinary atomic record links are still valid.
  // CHECK-LABEL: func.func private @atomic_record()
  // CHECK-SAME: !reussir.rc<!reussir.record<compound "inner" {i64}> atomic>}
  func.func private @atomic_record() -> !atomic_record
}

// RUN: %reussir-opt %s -split-input-file -verify-diagnostics
// A normal shared link to a record is derived from the member's capability;
// spelling it as an explicit rc member stays an error (only atomic links and
// links to an `ffi_object` are explicit).
module @test {
  // expected-error @+1 {{rc members must be atomic shared links or shared links to an ffi_object; a normal link is derived from the member's capability instead}}
  func.func private @foo() -> !reussir.record<compound "bad" {!reussir.rc<!reussir.record<compound "inner" {i64}>>}>
}

// -----

// An opaque element does not make a rigid or flex member link shared.
!ffi = !reussir.ffi_object<"Foreign", @drop>
// expected-error @+1 {{rc members must be atomic shared links or shared links to an ffi_object}}
func.func private @rigid() -> !reussir.record<compound "rigid" {!reussir.rc<!ffi rigid>}>

// -----

!ffi = !reussir.ffi_object<"Foreign", @drop>
// expected-error @+1 {{rc members must be atomic shared links or shared links to an ffi_object}}
func.func private @flex() -> !reussir.record<compound "flex" {!reussir.rc<!ffi flex>}>

// -----

// Atomicity cannot bypass the shared-capability requirement either.
!ffi = !reussir.ffi_object<"Foreign", @drop>
// expected-error @+1 {{rc members must be atomic shared links or shared links to an ffi_object}}
func.func private @atomic_rigid() -> !reussir.record<compound "atomic_rigid" {!reussir.rc<!ffi rigid atomic>}>

// -----

!ffi = !reussir.ffi_object<"Foreign", @drop>
// expected-error @+1 {{rc members must be atomic shared links or shared links to an ffi_object}}
func.func private @atomic_flex() -> !reussir.record<compound "atomic_flex" {!reussir.rc<!ffi flex atomic>}>

// -----

// Use a regional container, where [field] is otherwise permitted, so this
// checks the explicit-RC restriction rather than the shared-container rule.
!ffi = !reussir.ffi_object<"Foreign", @drop>
// expected-error @+1 {{an rc member cannot be a [field] link}}
func.func private @field_ffi() -> !reussir.record<compound "field_ffi" [regional] {[field] !reussir.rc<!ffi>}>

// -----

!ffi = !reussir.ffi_object<"Foreign", @drop>
// expected-error @+1 {{an rc member cannot be a [field] link}}
func.func private @field_atomic_ffi() -> !reussir.record<compound "field_atomic_ffi" [regional] {[field] !reussir.rc<!ffi atomic>}>

// -----

// The pre-existing ban on [field] atomic record links also remains.
!inner = !reussir.record<compound "inner" {i64}>
// expected-error @+1 {{an rc member cannot be a [field] link}}
func.func private @field_atomic_record() -> !reussir.record<compound "field_atomic_record" [regional] {[field] !reussir.rc<!inner atomic>}>

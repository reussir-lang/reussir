// RUN: %reussir-opt %s --reussir-instrument-nonlinear-ffi | %FileCheck %s

// A tagged ffi_object may be an immediate (an odd word, not a box), which
// has no reference count to read: the nonlinear-usage check skips such
// arguments and still checks untagged ones.

!word = !reussir.rc<!reussir.ffi_object<"::demo::Word", @word_drop, tagged = true>>
!vec = !reussir.rc<!reussir.ffi_object<"::demo::Vec", @vec_drop>>

module {
  func.func private @word_drop(!word)
  func.func private @vec_drop(!vec)
  func.func private @use_word(!word) -> i64
  func.func private @use_vec(!vec) -> i64
  reussir.trampoline import "C" @use_word_ffi = @use_word
  reussir.trampoline import "C" @use_vec_ffi = @use_vec

  // CHECK-LABEL: func.func @tagged
  // CHECK-NOT: reussir.rc.fetch
  // CHECK: call @use_word
  func.func @tagged(%w: !word) -> i64 {
    %r = func.call @use_word(%w) : (!word) -> i64
    return %r : i64
  }

  // CHECK-LABEL: func.func @untagged
  // CHECK: reussir.rc.fetch
  // CHECK: call @use_vec
  func.func @untagged(%v: !vec) -> i64 {
    %r = func.call @use_vec(%v) : (!vec) -> i64
    return %r : i64
  }
}

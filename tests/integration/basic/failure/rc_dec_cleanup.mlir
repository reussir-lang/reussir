// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

!handle = !reussir.rc<!reussir.ffi_object<"T", @drop>>
module {
  func.func private @drop(!handle)
  func.func @missing(%x: !handle) {
    // expected-error @+1 {{FFI object decrement requires cleanup}}
    reussir.rc.dec(%x : !handle)
    return
  }
}

// -----

!handle = !reussir.rc<!reussir.ffi_object<"T", @drop>>
module {
  func.func private @drop(!handle)
  func.func private @other(!handle)
  func.func @mismatch(%x: !handle) {
    // expected-error @+1 {{cleanup must match the FFI object's cleanup hook: @drop}}
    reussir.rc.dec(%x : !handle) cleanup(@other)
    return
  }
}

// -----

!handle = !reussir.rc<!reussir.ffi_object<"T", @missing>>
module {
  func.func @unresolved(%x: !handle) {
    // expected-error @+1 {{cleanup hook not found: @missing}}
    reussir.rc.dec(%x : !handle) cleanup(@missing)
    return
  }
}

// -----

!handle = !reussir.rc<!reussir.ffi_object<"T", @drop>>
module {
  func.func private @drop(i64)
  func.func @wrong_signature(%x: !handle) {
    // expected-error @+1 {{cleanup hook must be a function with one argument and no return type}}
    reussir.rc.dec(%x : !handle) cleanup(@drop)
    return
  }
}

// -----

module {
  func.func @non_ffi(%x: !reussir.rc<i64>) {
    // expected-error @+1 {{cleanup requires an FFI object decrement}}
    reussir.rc.dec(%x : !reussir.rc<i64>) cleanup(@drop)
    return
  }
}

// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

!handle = !reussir.rc<!reussir.ffi_object<"T", @missing>>
module {
  func.func @unresolved(%x: !handle) {
    // expected-error @+1 {{cleanup hook not found: @missing}}
    reussir.rc.dec(%x : !handle) cleanup(@missing)
    return
  }
}

// -----

module {
  func.func private @present()
  func.func @unresolved_second(%x: !reussir.rc<i64>) {
    // expected-error @+1 {{cleanup hook not found: @missing}}
    reussir.rc.dec(%x : !reussir.rc<i64>) cleanup(@present, @missing)
    return
  }
}

// -----

module {
  llvm.mlir.global external @not_function(0 : i64) : i64
  func.func @wrong_symbol_kind(%x: !reussir.rc<i64>) {
    // expected-error @+1 {{cleanup hook not found: @not_function}}
    reussir.rc.dec(%x : !reussir.rc<i64>) cleanup(@not_function)
    return
  }
}

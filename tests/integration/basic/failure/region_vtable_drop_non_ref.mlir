// RUN: %reussir-opt %s -verify-diagnostics

module @test {
  // Drop function with non-RefType parameter
  func.func private @drop_func_non_ref(%val : i32) -> () {
    return
  }
  
  // expected-error @+1 {{drop function input parameter must be RefType, got: 'i32'}}
  reussir.region.vtable @vtable1 {
    type(i32)
    drop(@drop_func_non_ref)
  }
}

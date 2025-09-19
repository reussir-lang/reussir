// RUN: %reussir-opt %s -verify-diagnostics

module @test {
  // Drop function with no inputs
  func.func private @drop_func_no_inputs() -> () {
    return
  }
  
  // expected-error @+1 {{drop function must have exactly one input parameter, got: 0}}
  reussir.region.vtable @vtable1 {
    type(i32)
    drop(@drop_func_no_inputs)
  }
}

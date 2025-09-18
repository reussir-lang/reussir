// RUN: %reussir-opt %s -verify-diagnostics

module @test {
  // Drop function with multiple inputs
  func.func private @drop_func_multiple_inputs(%ptr1 : !reussir.ref<i32>, %ptr2 : !reussir.ref<i32>) -> () {
    return
  }
  
  // expected-error @+1 {{drop function must have exactly one input parameter, got: 2}}
  reussir.region.vtable @vtable1 {
    type(i32)
    drop(@drop_func_multiple_inputs)
  }
}

// RUN: %reussir-opt %s -verify-diagnostics

module @test {
  // Drop function with wrong element type
  func.func private @drop_func_wrong_element_type(%ptr : !reussir.ref<f32>) -> () {
    return
  }
  
  reussir.region.vtable @vtable1 {
    // expected-error @-1 {{'reussir.region.vtable' op drop function input reference element type must match vtable type attribute, got: 'f32' but expected: 'i32'}}
    type(i32)
    drop(@drop_func_wrong_element_type)
  }
}

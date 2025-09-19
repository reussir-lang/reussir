// RUN: %reussir-opt %s -verify-diagnostics

module @test {
  // Drop function with wrong complex element type
  func.func private @drop_func_wrong_complex_type(%ptr : !reussir.ref<!reussir.record<compound "MyStruct1" {i32, f64}>>) -> () {
    return
  }
  
  reussir.region.vtable @vtable1 {
    // expected-error @-1 {{'reussir.region.vtable' op drop function input reference element type must match vtable type attribute, got: '!reussir.record<compound "MyStruct1" {i32, f64}>' but expected: '!reussir.record<compound "MyStruct2" {i32, i32}>'}}
    type(!reussir.record<compound "MyStruct2" {i32, i32}>)
    drop(@drop_func_wrong_complex_type)
  }
}

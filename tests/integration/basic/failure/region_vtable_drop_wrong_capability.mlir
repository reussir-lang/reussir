// RUN: %reussir-opt %s -verify-diagnostics

module @test {
  // Drop function with wrong capability
  func.func private @drop_func_wrong_capability(%ptr : !reussir.ref<i32 shared>) -> () {
    return
  }
  
  // expected-error @+1 {{drop function input parameter must have unspecified capability, got: shared}}
  reussir.region.vtable @vtable1 {
    size(8)
    alignment(8)
    drop(@drop_func_wrong_capability)
  }
}
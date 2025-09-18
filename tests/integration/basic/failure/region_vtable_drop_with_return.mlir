// RUN: %reussir-opt %s -verify-diagnostics

module @test {
  // Drop function with return value
  func.func private @drop_func_with_return(%ptr : !reussir.ref<i32>) -> i32 {
    %c1 = arith.constant 1 : i32
    return %c1 : i32
  }
  
  // expected-error @+1 {{drop function must have zero outputs, got: 1}}
  reussir.region.vtable @vtable1 {
    size(8)
    alignment(8)
    drop(@drop_func_with_return)
  }
}
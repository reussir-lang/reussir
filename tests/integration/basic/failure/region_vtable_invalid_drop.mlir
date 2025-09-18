// RUN: %reussir-opt %s -verify-diagnostics

module @test {
  // expected-error @+1 {{drop function not found: @nonexistent_func}}
  reussir.region.vtable @vtable1 {
    size(8)
    alignment(8)
    drop(@nonexistent_func)
  }
}

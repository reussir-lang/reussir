// RUN: %reussir-opt %s -verify-diagnostics
module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  func.func @test_function(%region: !reussir.region) {
    %token = reussir.token.alloc : !reussir.token<align: 8, size: 32>
    %val = arith.constant 42 : i64
    // expected-error @+1 {{vtable symbol not found: @nonexistent_vtable}}
    %rc_ptr = reussir.rc.create 
      value(%val : i64) 
      token(%token : !reussir.token<align: 8, size: 32>)
      region(%region : !reussir.region)
      vtable(@nonexistent_vtable)
      : !reussir.rc<i64 flex>
    return
  }
}
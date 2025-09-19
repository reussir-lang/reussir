// RUN: %reussir-opt %s -verify-diagnostics
module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  reussir.region.vtable @vtable1 {
    type(i64)
  }
  
  func.func @test_function(%region: !reussir.region) {
    %token = reussir.token.alloc : !reussir.token<align: 8, size: 32>
    %val = arith.constant 42 : i32
    // expected-error @+1 {{vtable type attribute must match value input type, vtable type: 'i64', value type: 'i32'}}
    %rc_ptr = reussir.rc.create 
      value(%val : i32) 
      token(%token : !reussir.token<align: 8, size: 32>)
      region(%region : !reussir.region)
      vtable(@vtable1)
      : !reussir.rc<i32 flex>
    return
  }
}
// RUN: %reussir-opt %s -verify-diagnostics
module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  reussir.region.vtable @vtable1 {
    type(i32)
  }
  
  func.func @test_function() {
    %token = reussir.token.alloc : !reussir.token<align: 8, size: 16>
    %val = arith.constant 42 : i32
    // expected-error @+1 {{when vtable is provided, region argument must also exist}}
    %rc_ptr = reussir.rc.create 
      value(%val : i32) 
      token(%token : !reussir.token<align: 8, size: 16>)
      vtable(@vtable1)
      : !reussir.rc<i32>
    return
  }
}
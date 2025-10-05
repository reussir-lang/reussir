// RUN: %reussir-opt %s -verify-diagnostics

// Test failure: closure clone with mismatched types
module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  func.func private @test_closure_clone_type_mismatch() -> !reussir.rc<!reussir.closure<(i64) -> i64>> {
    %token = reussir.token.alloc : !reussir.token<align: 8, size: 32>
    %original = reussir.closure.create -> !reussir.rc<!reussir.closure<(i32) -> i32>> {
      token(%token : !reussir.token<align: 8, size: 32>)
      body {
        ^bb0(%v0 : i32):
          %one = arith.constant 1 : i32
          %add = arith.addi %v0, %one : i32 
          reussir.closure.yield %add : i32
      }
    }
    // Clone with mismatched types - should fail
    // expected-error @+1 {{input and output closure types must be the same, input type: '!reussir.rc<!reussir.closure<(i32) -> i32>>', output type: '!reussir.rc<!reussir.closure<(i64) -> i64>>'}}
    %cloned = reussir.closure.clone (%original : !reussir.rc<!reussir.closure<(i32) -> i32>>) : !reussir.rc<!reussir.closure<(i64) -> i64>>
    return %cloned : !reussir.rc<!reussir.closure<(i64) -> i64>>
  }
}

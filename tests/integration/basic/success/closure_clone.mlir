// RUN: %reussir-opt %s  | %reussir-opt

// Test closure clone with matching types
module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  func.func private @test_closure_clone() -> !reussir.rc<!reussir.closure<(i32) -> i32>> {
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
    // Clone with matching types - should pass
    %cloned = reussir.closure.clone (%original : !reussir.rc<!reussir.closure<(i32) -> i32>>) : !reussir.rc<!reussir.closure<(i32) -> i32>>
    return %cloned : !reussir.rc<!reussir.closure<(i32) -> i32>>
  }
}

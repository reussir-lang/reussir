// RUN: %reussir-opt %s --reussir-token-allocation | %FileCheck %s

module attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>> } {
  
  // Test: rc.create without token should get token allocated
  // CHECK-LABEL: func.func @rc_create_without_token
  // CHECK: reussir.token.alloc
  // CHECK: reussir.rc.create
  // CHECK-SAME: token(
  func.func @rc_create_without_token(%value: i64) -> !reussir.rc<i64> {
    %rc = reussir.rc.create value(%value : i64) : !reussir.rc<i64>
    return %rc : !reussir.rc<i64>
  }

  // Test: rc.create with token should remain unchanged
  // CHECK-LABEL: func.func @rc_create_with_token
  // CHECK: reussir.rc.create
  // CHECK-SAME: token(%arg1
  // CHECK-NOT: reussir.token.alloc
  func.func @rc_create_with_token(%value: i64, %token: !reussir.token<align: 8, size: 16>) -> !reussir.rc<i64> {
    %rc = reussir.rc.create value(%value : i64) token(%token : !reussir.token<align: 8, size: 16>) : !reussir.rc<i64>
    return %rc : !reussir.rc<i64>
  }

  // Test: rc.create with region and without token
  // CHECK-LABEL: func.func @rc_create_region_without_token
  // CHECK-NEXT: reussir.token.alloc
  // CHECK-NEXT: reussir.rc.create
  func.func @rc_create_region_without_token(%value: i64, %region: !reussir.region) -> !reussir.rc<i64 flex> {
    %rc = reussir.rc.create value(%value : i64) region(%region : !reussir.region) : !reussir.rc<i64 flex>
    return %rc : !reussir.rc<i64 flex>
  }

  reussir.closure.vtable @VTable {
    func(@add_one)
    closure(!reussir.closure<(i32) -> i32>)
  }

  func.func private @add_one(%v0 : i32) -> i32 {
    %one = arith.constant 1 : i32
    %add = arith.addi %v0, %one : i32
    return %add : i32
  }

  // Test: closure.create without token should get token allocated (inlined)
  // CHECK-LABEL: func.func @closure_create_without_token_inlined
  // CHECK-NEXT: reussir.token.alloc
  // CHECK-NEXT: reussir.closure.create
  func.func @closure_create_without_token_inlined() -> !reussir.rc<!reussir.closure<(i32) -> i32>> {
    %closure = reussir.closure.create -> !reussir.rc<!reussir.closure<(i32) -> i32>> {
      body {
        ^bb0(%v0 : i32):
          %one = arith.constant 1 : i32
          %add = arith.addi %v0, %one : i32
          reussir.closure.yield %add : i32
      }
    }
    return %closure : !reussir.rc<!reussir.closure<(i32) -> i32>>
  }

  // Test: closure.create without token should get token allocated (outlined)
  // CHECK-LABEL: func.func @closure_create_without_token_outlined
  // CHECK-NEXT: reussir.token.alloc
  // CHECK-NEXT: reussir.closure.create
  func.func @closure_create_without_token_outlined() -> !reussir.rc<!reussir.closure<(i32) -> i32>> {
    %closure = reussir.closure.create -> !reussir.rc<!reussir.closure<(i32) -> i32>> {
      vtable(@VTable)
    }
    return %closure : !reussir.rc<!reussir.closure<(i32) -> i32>>
  }

  // Test: closure.create with token should remain unchanged
  // CHECK-LABEL: func.func @closure_create_with_token
  // CHECK-NEXT: reussir.closure.create
  // CHECK-NEXT: token (%arg0
  // CHECK-NOT: reussir.token.alloc
  func.func @closure_create_with_token(%token: !reussir.token<align: 8, size: 32>) -> !reussir.rc<!reussir.closure<(i32) -> i32>> {
    %closure = reussir.closure.create -> !reussir.rc<!reussir.closure<(i32) -> i32>> {
      token(%token : !reussir.token<align: 8, size: 32>)
      body {
        ^bb0(%v0 : i32):
          %one = arith.constant 1 : i32
          %add = arith.addi %v0, %one : i32
          reussir.closure.yield %add : i32
      }
    }
    return %closure : !reussir.rc<!reussir.closure<(i32) -> i32>>
  }
}

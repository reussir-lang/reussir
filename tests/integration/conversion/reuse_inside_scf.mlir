// RUN: %reussir-opt %s -reussir-token-reuse | %FileCheck %s
!rc64 = !reussir.rc<i64>
!i64token = !reussir.token<align: 8, size: 16>

module @test attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<f80, dense<128> : vector<2xi64>>, #dlti.dl_entry<i128, dense<128> : vector<2xi64>>, #dlti.dl_entry<i32, dense<32> : vector<2xi64>>, #dlti.dl_entry<f128, dense<128> : vector<2xi64>>, #dlti.dl_entry<f64, dense<64> : vector<2xi64>>, #dlti.dl_entry<f16, dense<16> : vector<2xi64>>, #dlti.dl_entry<i1, dense<8> : vector<2xi64>>, #dlti.dl_entry<!llvm.ptr, dense<64> : vector<4xi64>>, #dlti.dl_entry<!llvm.ptr<270>, dense<32> : vector<4xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>, #dlti.dl_entry<i16, dense<16> : vector<2xi64>>, #dlti.dl_entry<!llvm.ptr<272>, dense<64> : vector<4xi64>>, #dlti.dl_entry<!llvm.ptr<271>, dense<32> : vector<4xi64>>, #dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<"dlti.stack_alignment", 128 : i64>, #dlti.dl_entry<"dlti.endianness", "little">>, llvm.data_layout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"} {
    // CHECK-LABEL: func.func @reuse
    // CHECK-SAME: (%[[regarg0:[a-z0-9]+]]: !reussir.rc<i64>, %[[regarg1:[a-z0-9]+]]: i1, %[[regarg2:[a-z0-9]+]]: i64, %[[regarg3:[a-z0-9]+]]: i64) -> !reussir.rc<i64> {
    // CHECK:  %[[reg0:[a-z0-9]+]] = reussir.rc.dec(%[[regarg0]] : !reussir.rc<i64>) : !reussir.nullable<!reussir.token<align : 8, size : 16>>
    // CHECK:  %[[reg1:[a-z0-9]+]] = scf.if %[[regarg1]] -> (!reussir.rc<i64>) {
    // CHECK:    %[[reg2:[a-z0-9]+]] = reussir.token.ensure(%[[reg0]] : <!reussir.token<align : 8, size : 16>>) : <align : 8, size : 16>
    // CHECK:    %[[reg3:[a-z0-9]+]] = reussir.rc.create value(%[[regarg2]] : i64) token(%[[reg2]] : !reussir.token<align : 8, size : 16>) : !reussir.rc<i64>
    // CHECK:    scf.yield %[[reg3]] : !reussir.rc<i64>
    // CHECK:  } else {
    // CHECK:    %[[reg2:[a-z0-9]+]] = reussir.token.ensure(%[[reg0]] : <!reussir.token<align : 8, size : 16>>) : <align : 8, size : 16>
    // CHECK:    %[[reg3:[a-z0-9]+]] = reussir.rc.create value(%[[regarg3]] : i64) token(%[[reg2]] : !reussir.token<align : 8, size : 16>) : !reussir.rc<i64>
    // CHECK:    scf.yield %[[reg3]] : !reussir.rc<i64>
    // CHECK:  }
    // CHECK:  return %[[reg1]] : !reussir.rc<i64>
    // CHECK: }
    func.func @reuse(%0: !rc64, %1: i1, %x: i64, %y: i64) -> !rc64 {
        %2 = reussir.rc.dec (%0 : !rc64) : !reussir.nullable<!reussir.token<align: 8, size: 16>>
        %3 = scf.if %1 -> !rc64 {
          %tk = reussir.token.alloc : !i64token
          %4 = reussir.rc.create value(%x : i64) token(%tk : !i64token) : !rc64
          scf.yield %4 : !rc64  
        } else {
          %tk = reussir.token.alloc : !i64token
          %5 = reussir.rc.create value(%y : i64) token(%tk : !i64token) : !rc64
          scf.yield %5 : !rc64  
        }
        return %3 : !rc64
    }

    func.func private @opaque(%0: !rc64)
    // CHECK-LABEL: func.func @partial
    // CHECK-SAME: (%[[regarg0:[a-z0-9]+]]: !reussir.rc<i64>, %[[regarg1:[a-z0-9]+]]: i1, %[[regarg2:[a-z0-9]+]]: i64) {
    // CHECK:  %[[reg0:[a-z0-9]+]] = reussir.rc.dec(%[[regarg0]] : !reussir.rc<i64>) : !reussir.nullable<!reussir.token<align : 8, size : 16>>
    // CHECK:  scf.if %[[regarg1]] {
    // CHECK:    %[[reg1:[a-z0-9]+]] = reussir.token.ensure(%[[reg0]] : <!reussir.token<align : 8, size : 16>>) : <align : 8, size : 16>
    // CHECK:    %[[reg2:[a-z0-9]+]] = reussir.rc.create value(%[[regarg2]] : i64) token(%[[reg1]] : !reussir.token<align : 8, size : 16>) : !reussir.rc<i64>
    // CHECK:    func.call @opaque(%[[reg2]]) : (!reussir.rc<i64>) -> ()
    // CHECK:  } else {
    // CHECK:    reussir.token.free(%[[reg0]] : !reussir.nullable<!reussir.token<align : 8, size : 16>>)
    // CHECK:  }
    // CHECK:  return
    // CHECK: }
    func.func @partial(%0: !rc64, %1: i1, %x: i64) {
        %2 = reussir.rc.dec (%0 : !rc64) : !reussir.nullable<!reussir.token<align: 8, size: 16>>
        scf.if %1 {
          %tk = reussir.token.alloc : !i64token
          %5 = reussir.rc.create value(%x : i64) token(%tk : !i64token) : !rc64
          func.call @opaque(%5) : (!rc64) -> ()
          scf.yield
        } else {
          scf.yield 
        }
        return
    }
}

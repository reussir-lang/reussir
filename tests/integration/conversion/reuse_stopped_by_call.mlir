// RUN: %reussir-opt %s -reussir-token-reuse | %FileCheck %s
!rc64 = !reussir.rc<i64>
!rc64x2 = !reussir.rc<!reussir.record<compound "test" {i64, i64}>>
!i64token = !reussir.token<align: 8, size: 16>

module @test attributes {dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<f80, dense<128> : vector<2xi64>>, #dlti.dl_entry<i128, dense<128> : vector<2xi64>>, #dlti.dl_entry<i32, dense<32> : vector<2xi64>>, #dlti.dl_entry<f128, dense<128> : vector<2xi64>>, #dlti.dl_entry<f64, dense<64> : vector<2xi64>>, #dlti.dl_entry<f16, dense<16> : vector<2xi64>>, #dlti.dl_entry<i1, dense<8> : vector<2xi64>>, #dlti.dl_entry<!llvm.ptr, dense<64> : vector<4xi64>>, #dlti.dl_entry<!llvm.ptr<270>, dense<32> : vector<4xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>, #dlti.dl_entry<i16, dense<16> : vector<2xi64>>, #dlti.dl_entry<!llvm.ptr<272>, dense<64> : vector<4xi64>>, #dlti.dl_entry<!llvm.ptr<271>, dense<32> : vector<4xi64>>, #dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<"dlti.stack_alignment", 128 : i64>, #dlti.dl_entry<"dlti.endianness", "little">>, llvm.data_layout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"} {
    func.func private @opaque()

    func.func @reuse(%0: !rc64){
        %1 = reussir.rc.dec (%0 : !rc64) : !reussir.nullable<!reussir.token<align: 8, size: 16>>
        // CHECK: reussir.token.free
        // CHECK-NEXT: call @opaque
        func.call @opaque() : () -> ()
        return
    }

    func.func @partial(%0: !rc64, %1: i1, %x: i64) {
        %2 = reussir.rc.dec (%0 : !rc64) : !reussir.nullable<!reussir.token<align: 8, size: 16>>
        scf.if %1 {
        // CHECK:      %[[a:[a-z0-9]+]] = reussir.token.ensure(%{{[a-z0-9]+}} : <!reussir.token<align : 8, size : 16>>) : <align : 8, size : 16>
        // CHECK-NEXT: %{{[a-z0-9]+}} = reussir.rc.create value(%{{[a-z0-9]+}} : i64) token(%[[a]] : !reussir.token<align : 8, size : 16>) : !reussir.rc<i64>
          %tk = reussir.token.alloc : !i64token
          %5 = reussir.rc.create value(%x : i64) token(%tk : !i64token) : !rc64
          scf.yield
        } else {
          // CHECK: reussir.token.free
          // CHECK-NEXT: call @opaque
          func.call @opaque() : () -> ()
          scf.yield 
        }
        return
    }

    func.func @free(%0: !rc64, %1: i1, %x: i64) {
        %2 = reussir.rc.dec (%0 : !rc64) : !reussir.nullable<!reussir.token<align: 8, size: 16>>
        // CHECK: reussir.token.free
        // CHECK-NEXT: } else {
        // CHECK-NEXT: reussir.token.free
        scf.if %1 {
          scf.yield
        } else {
          func.call @opaque() : () -> ()
          scf.yield
        }
        return
    }
}

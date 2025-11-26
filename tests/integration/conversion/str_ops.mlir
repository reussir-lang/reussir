// RUN: %reussir-opt %s --reussir-lowering-basic-ops
module @test attributes { dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>>} {
  // CHECK: llvm.mlir.global linkonce_odr constant @hello("Hello, World!\00") {addr_space = 0 : i32}
  reussir.str.global @hello = "Hello, World!"

  // CHECK: llvm.mlir.global linkonce_odr constant @empty("\00") {addr_space = 0 : i32}
  reussir.str.global @empty = ""

  // CHECK-LABEL: llvm.func @test_str_literal() -> !llvm.struct<(ptr, i64)>
  // CHECK: %[[ADDR:.*]] = llvm.mlir.addressof @hello : !llvm.ptr
  // CHECK: %[[LEN:.*]] = arith.constant 13 : i64
  // CHECK: %[[UNDEF:.*]] = llvm.mlir.undef : !llvm.struct<(ptr, i64)>
  // CHECK: %[[WITH_PTR:.*]] = llvm.insertvalue %[[ADDR]], %[[UNDEF]][0]
  // CHECK: %[[WITH_LEN:.*]] = llvm.insertvalue %[[LEN]], %[[WITH_PTR]][1]
  // CHECK: llvm.return %[[WITH_LEN]]
  func.func @test_str_literal() -> !reussir.str<global> {
    %str = reussir.str.literal @hello : !reussir.str<global>
    return %str : !reussir.str<global>
  }

  // CHECK-LABEL: llvm.func @test_empty_str_literal() -> !llvm.struct<(ptr, i64)>
  // CHECK: %[[ADDR:.*]] = llvm.mlir.addressof @empty : !llvm.ptr
  // CHECK: %[[LEN:.*]] = arith.constant 0 : i64
  func.func @test_empty_str_literal() -> !reussir.str<global> {
    %str = reussir.str.literal @empty : !reussir.str<global>
    return %str : !reussir.str<global>
  }
}


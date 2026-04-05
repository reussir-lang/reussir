// Pass twice to make sure the output is stable
// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s
module @test {
  // CHECK: func.func private @foo() -> !reussir.ref<index>
  func.func private @foo() -> !reussir.ref<index unspecified normal>

  // CHECK: func.func private @bar() -> !reussir.ref<index rigid>
  func.func private @bar() -> !reussir.ref<index rigid normal>

  // CHECK: func.func private @baz() -> !reussir.ref<index shared atomic>
  func.func private @baz() -> !reussir.ref<index shared atomic>

  // CHECK: func.func private @qux() -> !reussir.ref<index shared atomic>
  func.func private @qux() -> !reussir.ref<index atomic shared>

  // CHECK: func.func private @bridge(%[[REF:.*]]: !reussir.ref<i64>) -> !reussir.ref<i64>
  // CHECK: %[[VIEW:.*]] = reussir.ref.to_memref(%[[REF]] : !reussir.ref<i64>) : memref<i64>
  // CHECK: %[[ROUNDTRIP:.*]] = reussir.ref.from_memref(%[[VIEW]] : memref<i64>) : !reussir.ref<i64>
  // CHECK: return %[[ROUNDTRIP]] : !reussir.ref<i64>
  func.func private @bridge(%ref : !reussir.ref<i64>) -> !reussir.ref<i64> {
    %view = reussir.ref.to_memref (%ref : !reussir.ref<i64>) : memref<i64>
    %roundtrip = reussir.ref.from_memref (%view : memref<i64>) : !reussir.ref<i64>
    return %roundtrip : !reussir.ref<i64>
  }
}

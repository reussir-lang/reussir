// RUN: %reussir-opt %s | %FileCheck %s
module @test {
  // CHECK: reussir.region.vtable @vtable1
  reussir.region.vtable @vtable1 {
    type(i32)
  }
}

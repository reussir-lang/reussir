// RUN: %reussir-opt %s | %FileCheck %s
module @test {
  // CHECK: func.func private @drop_func
  func.func private @drop_func(%ptr : !reussir.ref<i32>) -> () {
    return
  }
  
  // CHECK: reussir.region.vtable @vtable1
  reussir.region.vtable @vtable1 {
    size(8)
    alignment(8)
    drop(@drop_func)
  }
  
  // CHECK: reussir.region.vtable @vtable2
  reussir.region.vtable @vtable2 {
    size(16)
    alignment(8)
    scan_offsets [0, 8]
  }
  
  // CHECK: reussir.region.vtable @vtable3
  reussir.region.vtable @vtable3 {
    size(24)
    alignment(8)
    drop(@drop_func)
    scan_offsets [0, 8, 16]
  }
}

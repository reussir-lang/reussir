// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s

!P = !ptr.ptr<#ptr.generic_space>

// CHECK-LABEL: func.func @address_ops
// CHECK: %[[ADDR:.*]] = reussir.ptr.addr %{{.*}} : !ptr.ptr<#ptr.generic_space> -> i64
// CHECK: %[[MASKED:.*]] = reussir.ptr.mask %{{.*}}, %[[ADDR]] : !ptr.ptr<#ptr.generic_space>, i64
// CHECK: reussir.ptr.from_addr %{{.*}} : index -> !ptr.ptr<#ptr.generic_space>
func.func @address_ops(%p: !P, %i: index) -> (!P, !P) {
  %addr = reussir.ptr.addr %p : !P -> i64
  %masked = reussir.ptr.mask %p, %addr : !P, i64
  %q = reussir.ptr.from_addr %i : index -> !P
  return %masked, %q : !P, !P
}

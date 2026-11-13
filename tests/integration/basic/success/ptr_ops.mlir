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

// CHECK-LABEL: func.func @provenance_ops
// CHECK: reussir.ptr.expose_addr %{{.*}} : !ptr.ptr<#ptr.generic_space> -> i64
// CHECK: %[[FRESH:.*]] = reussir.ptr.launder_invariant_group %{{.*}} : !ptr.ptr<#ptr.generic_space>
// CHECK: reussir.ptr.strip_invariant_group %[[FRESH]] : !ptr.ptr<#ptr.generic_space>
func.func @provenance_ops(%p: !P) -> (i64, !P) {
  %addr = reussir.ptr.expose_addr %p : !P -> i64
  %fresh = reussir.ptr.launder_invariant_group %p : !P
  %plain = reussir.ptr.strip_invariant_group %fresh : !P
  return %addr, %plain : i64, !P
}

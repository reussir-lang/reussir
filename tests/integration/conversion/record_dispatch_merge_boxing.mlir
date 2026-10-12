// Canonicalization does not merge regions of a `reussir.record.dispatch`
// that box a value of the dispatched record's type: each per-tag copy can
// then reuse its own tag's cell in place, where a region over several tags
// would release a cell of unknown size and reuse it through a reallocation.
// Regions without such a construction are merged as usual.

// RUN: %reussir-opt %s --canonicalize | %FileCheck %s

!variant = !reussir.record<variant "Wide" {i32, i64, i32, i64, i32}>
!ref = !reussir.ref<!variant field>
#wild = loc("wild.rr":7:14)
module {
  func.func private @bump(%counter: memref<i32>)

  // CHECK-LABEL: func.func @boxes_scrutinee_type
  // CHECK: [0, 1] -> {
  // CHECK: reussir.rc.create
  // CHECK: [2, 3] -> {
  // CHECK: [4] -> {
  // CHECK: reussir.rc.create
  func.func @boxes_scrutinee_type(%ref: !ref, %v: !variant, %counter: memref<i32>) {
    reussir.record.dispatch(%ref : !ref) {
      [0, 1] -> {
        %b = reussir.rc.create value(%v : !variant) : !reussir.rc<!variant> loc(#wild)
        reussir.scf.yield loc(#wild)
      }
      [2] -> {
      ^bb0(%p: !reussir.ref<i32 field>):
        func.call @bump(%counter) : (memref<i32>) -> () loc(#wild)
        reussir.scf.yield loc(#wild)
      }
      [3] -> {
      ^bb0(%p: !reussir.ref<i64 field>):
        func.call @bump(%counter) : (memref<i32>) -> () loc(#wild)
        reussir.scf.yield loc(#wild)
      }
      [4] -> {
      ^bb0(%p: !reussir.ref<i32 field>):
        %b = reussir.rc.create value(%v : !variant) : !reussir.rc<!variant> loc(#wild)
        reussir.scf.yield loc(#wild)
      }
    }
    return
  }
}

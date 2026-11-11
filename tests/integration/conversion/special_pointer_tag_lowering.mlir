// RUN: %reussir-opt %s --convert-to-llvm | \
// RUN: %reussir-translate --mlir-to-llvmir | %FileCheck %s

// LLVM lowering under the special-pointer-tag scheme (module stamped with
// `reussir.special_ptr_tag`). A tagged immediate's top byte is `tag + 1`
// and its low bits hold the address of the per-tag dummy box
// `{refcount = 2, tag}`. TBI makes loads through the tagged value land in
// the dummy box. Refcount reads remain plain loads; record.tag decodes
// the top byte for immediates and loads the header for ordinary boxes.
// `rc.set`, which could decay the dummy's refcount to 1, skips a tagged
// access behind an unlikely branch (a select-steered store address would
// data-depend on the count and serialize every rc op against its load).

!list = !reussir.record<variant "List" {!reussir.record<compound "List.Cons" [value] {i64, !reussir.record<variant "List">}>, !reussir.record<compound "List.Nil" [value] {}>}>
!rclist = !reussir.rc<!list>

// CHECK-DAG: @_RNvC17REUSSIR_TAG_DUMMY43JTSaUNTZpqgb8a0JnRra5ebq8TvOJDFg1m5Smu4IYVX = linkonce_odr global [2 x i32] [i32 2, i32 1]
module @test attributes { reussir.special_ptr_tag = "tbi", dlti.dl_spec = #dlti.dl_spec<#dlti.dl_entry<i64, dense<64> : vector<2xi64>>, #dlti.dl_entry<i8, dense<8> : vector<2xi64>>> } {

  // A known nullary tag needs only the top byte, not its dummy box.
  // CHECK-LABEL: define i1 @is_nil(ptr %0)
  // CHECK-NOT: load
  // CHECK-NOT: REUSSIR_TAG_DUMMY
  // CHECK: %[[INT:.+]] = ptrtoint ptr %0 to i64
  // CHECK: %[[TOP:.+]] = lshr i64 %[[INT]], 56
  // CHECK: %[[ISNIL:.+]] = icmp eq i64 %[[TOP]], 2
  // CHECK-NEXT: ret i1 %[[ISNIL]]
  func.func @is_nil(%rc: !rclist) -> i1 {
    %nil = reussir.rc.compare_immortal(%rc : !rclist) tag(1)
    return %nil : i1
  }

  // Immediate materialization: dummy address | (tag + 1) << 56, no
  // allocation. (2 << 56 = 144115188075855872.)
  // CHECK-LABEL: define ptr @make_nil()
  // CHECK: %[[ENC:.+]] = or i64 ptrtoint (ptr @_RNvC17REUSSIR_TAG_DUMMY43JTSaUNTZpqgb8a0JnRra5ebq8TvOJDFg1m5Smu4IYVX to i64), 144115188075855872
  // CHECK: %[[PTR:.+]] = inttoptr i64 %[[ENC]] to ptr
  // CHECK: ret ptr %[[PTR]]
  func.func @make_nil() -> !rclist {
    %0 = reussir.rc.tagged tag(1) : !rclist
    return %0 : !rclist
  }

  // Hot read: plain refcount load, no top-byte decode, no select.
  // CHECK-LABEL: define i64 @fetch(ptr %0)
  // CHECK-NOT: lshr
  // CHECK-NOT: select
  // CHECK: %[[CNT:.+]] = load i32, ptr
  // CHECK: %[[CNTW:.+]] = zext i32 %[[CNT]] to i64
  // CHECK: ret i64 %[[CNTW]]
  func.func @fetch(%rc: !rclist) -> index {
    %cnt = reussir.rc.fetch (%rc : !rclist) : index
    return %cnt : index
  }

  // Decode immediate tags; the likely ordinary-box path reads the header.
  // CHECK-LABEL: define i64 @tag(ptr %0)
  // CHECK: %[[INT:.+]] = ptrtoint ptr %0 to i64
  // CHECK: %[[TOP:.+]] = lshr i64 %[[INT]], 56
  // CHECK: %[[ISTAG:.+]] = icmp ne i64 %[[TOP]], 0
  // CHECK: %[[DECODED:.+]] = sub i64 %[[TOP]], 1
  // CHECK: br i1 %[[ISTAG]], label %[[CONT:.+]], label %[[LOAD:.+]], !prof ![[TAG_WEIGHTS:[0-9]+]]
  // CHECK: [[LOAD]]:
  // CHECK: %[[TAG:.+]] = load i32, ptr
  // CHECK: %[[WIDE:.+]] = zext i32 %[[TAG]] to i64
  // CHECK: br label %[[CONT]]
  // CHECK: [[CONT]]:
  // CHECK: %[[RESULT:.+]] = phi i64 [ %[[WIDE]], %[[LOAD]] ], [ %[[DECODED]], %{{.+}} ]
  // CHECK: ret i64 %[[RESULT]]
  func.func @tag(%ref: !reussir.ref<!list shared>) -> index {
    %tag = reussir.record.tag(%ref : !reussir.ref<!list shared>) : index
    return %tag : index
  }

  // The decrementing store: a tagged access skips the store entirely via an
  // unlikely branch, so the dummy refcount is never written and can never
  // decay below 2.
  // CHECK-LABEL: define void @set(ptr %0, i64 %1)
  // CHECK: %[[NARROW:.+]] = trunc i64 %1 to i32
  // CHECK: %[[INT:.+]] = ptrtoint ptr %0 to i64
  // CHECK: %[[TOP:.+]] = lshr i64 %[[INT]], 56
  // CHECK: %[[ISTAG:.+]] = icmp ne i64 %[[TOP]], 0
  // CHECK: br i1 %[[ISTAG]], label %[[CONT:.+]], label %[[STORE:.+]], !prof
  // CHECK: [[STORE]]:
  // CHECK: store i32 %[[NARROW]], ptr %0
  // CHECK: br label %[[CONT]]
  func.func @set(%rc: !rclist, %v: index) {
    reussir.rc.set(%rc : !rclist, %v : index)
    return
  }

  // The increment stays a plain load/add/store, landing on the dummy for an
  // immediate. Its count then grows and wraps, so `old >= 1` is only assumed
  // for real boxes (zero top byte).
  // CHECK-LABEL: define void @inc(ptr %0)
  // CHECK: %[[OLD:.+]] = load i32, ptr %[[CNTP:.+]], align 4
  // CHECK: %[[NEW:.+]] = add i32 %[[OLD]], 1
  // CHECK: store i32 %[[NEW]], ptr %[[CNTP]]
  // CHECK: %[[GE:.+]] = icmp uge i32 %[[OLD]], 1
  // CHECK: %[[INT:.+]] = ptrtoint ptr %0 to i64
  // CHECK: %[[TOP:.+]] = lshr i64 %[[INT]], 56
  // CHECK: %[[ISTAG:.+]] = icmp ne i64 %[[TOP]], 0
  // CHECK: %[[OK:.+]] = or i1 %[[GE]], %[[ISTAG]]
  // CHECK: call void @llvm.assume(i1 %[[OK]])
  func.func @inc(%rc: !rclist) {
    reussir.rc.inc (%rc : !rclist)
    return
  }
}

// CHECK: ![[TAG_WEIGHTS]] = !{!"branch_weights", i32 1, i32 2000}

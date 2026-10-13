// RUN: %reussir-opt %s --reussir-acquire-drop-expansion='outline-record=true' | %FileCheck %s --check-prefix=OUTLINE
// RUN: %reussir-opt %s --reussir-acquire-drop-expansion | %FileCheck %s --check-prefix=INLINE
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-acquire-drop-expansion{outline-record=true},reussir-convert-to-std,convert-scf-to-cf,reussir-convert-to-llvm)' | %reussir-translate --mlir-to-llvmir | %FileCheck %s --check-prefix=LLVM

// Each DAG level contains the previous record twice. Outlined acquisition
// should share the child body, while the early inline expansion remains inline.
!payload = !reussir.record<compound "Payload" [shared] { i64 }>
!leaf = !reussir.record<compound "Leaf" [value] { !payload }>
!pair = !reussir.record<compound "Pair" [value] { !leaf, !leaf }>
!quad = !reussir.record<compound "Quad" [value] { !pair, !pair }>

module {
  // Glue is inserted before its caller. Both occurrences must call the same
  // child symbol and forward the dynamic delta unchanged.
  // OUTLINE-LABEL: func.func private @_RINvNvC4core9intrinsic16acquire_in_placeLeafE
  // OUTLINE: reussir.rc.inc({{.*}}) by %arg1
  // OUTLINE-LABEL: func.func private @_RINvNvC4core9intrinsic16acquire_in_placePairE
  // OUTLINE: call @_RINvNvC4core9intrinsic16acquire_in_placeLeafE(%{{.*}}, %arg1)
  // OUTLINE: call @_RINvNvC4core9intrinsic16acquire_in_placeLeafE(%{{.*}}, %arg1)
  // OUTLINE-LABEL: func.func private @_RINvNvC4core9intrinsic16acquire_in_placeQuadE
  // OUTLINE: call @_RINvNvC4core9intrinsic16acquire_in_placePairE(%{{.*}}, %arg1)
  // OUTLINE: call @_RINvNvC4core9intrinsic16acquire_in_placePairE(%{{.*}}, %arg1)
  // OUTLINE-LABEL: func.func private @_RINvNvC4core9intrinsic23acquire_in_place_atomicLeafE
  // OUTLINE: reussir.rc.inc({{.*}} atomic>) by %arg1
  // OUTLINE-LABEL: func.func private @_RINvNvC4core9intrinsic23acquire_in_place_atomicPairE
  // OUTLINE-NOT: reussir.rc.inc
  // OUTLINE: call @_RINvNvC4core9intrinsic23acquire_in_place_atomicLeafE(%{{.*}}, %arg1)
  // OUTLINE: call @_RINvNvC4core9intrinsic23acquire_in_place_atomicLeafE(%{{.*}}, %arg1)
  // OUTLINE-NOT: reussir.rc.inc
  // OUTLINE-LABEL: func.func private @_RINvNvC4core9intrinsic23acquire_in_place_atomicQuadE
  // OUTLINE: call @_RINvNvC4core9intrinsic23acquire_in_place_atomicPairE(%{{.*}}, %arg1)
  // OUTLINE: call @_RINvNvC4core9intrinsic23acquire_in_place_atomicPairE(%{{.*}}, %arg1)

  // LLVM-LABEL: define linkonce_odr void @_RINvNvC4core9intrinsic16acquire_in_placeLeafE
  // LLVM-NOT: atomicrmw
  // LLVM: store i32
  // LLVM: ret void
  // LLVM-LABEL: define linkonce_odr void @_RINvNvC4core9intrinsic23acquire_in_place_atomicLeafE
  // LLVM: atomicrmw add ptr {{.*}}, i32 {{.*}} monotonic
  // LLVM-LABEL: define linkonce_odr void @_RINvNvC4core9intrinsic23acquire_in_place_atomicPairE
  // LLVM: call void @_RINvNvC4core9intrinsic23acquire_in_place_atomicLeafE
  // LLVM: call void @_RINvNvC4core9intrinsic23acquire_in_place_atomicLeafE

  // INLINE-LABEL: func.func @normal
  // INLINE-NOT: func.call
  // INLINE-COUNT-4: reussir.rc.inc({{.*}}) by %arg1
  // INLINE-NOT: reussir.rc.inc
  // INLINE: return
  func.func @normal(%ref: !reussir.ref<!quad>, %delta: index) {
    reussir.ref.acquire(%ref : !reussir.ref<!quad>) by %delta
    return
  }
  // INLINE-LABEL: func.func @atomic
  // INLINE-NOT: func.call
  // INLINE-COUNT-4: reussir.rc.inc({{.*}} atomic>) by %arg1
  // INLINE: return
  func.func @atomic(%ref: !reussir.ref<!quad atomic>, %delta: index) {
    reussir.ref.acquire(%ref : !reussir.ref<!quad atomic>) by %delta
    return
  }
}

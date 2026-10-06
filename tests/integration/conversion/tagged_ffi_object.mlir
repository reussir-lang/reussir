// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s --check-prefix=PARSE
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-lowering-basic-ops --convert-to-llvm --reconcile-unrealized-casts | %reussir-translate --mlir-to-llvmir | %FileCheck %s

// A `tagged` ffi_object handle may be an odd immediate word (the foreign
// side encodes a small value in the handle itself, as Lean boxes scalars):
// `rc.inc` updates the refcount and `rc.dec` calls the cleanup hook only
// when the low bit is clear, and nothing is loaded before that test. The
// top 16 bits of a real box's handle may carry foreign data: `rc.inc`
// clears them (a pointer mask) before it touches the refcount, also with
// a delta and for an atomic count; the cleanup hook gets the handle
// unchanged. An untagged
// ffi_object keeps the unguarded forms.

!tagged = !reussir.rc<!reussir.ffi_object<"::demo::Word", @word_drop, tagged = true>>
!plain = !reussir.rc<!reussir.ffi_object<"::demo::Box", @box_drop>>
!taggedA = !reussir.rc<!reussir.ffi_object<"::demo::Word", @word_drop_atomic, tagged = true> atomic>

// PARSE: !reussir.ffi_object<"::demo::Word", @word_drop, tagged = true>
// PARSE: !reussir.ffi_object<"::demo::Box", @box_drop>>

module {
  func.func private @word_drop(!tagged)
  func.func private @box_drop(!plain)
  func.func private @word_drop_atomic(!taggedA)

  // CHECK-LABEL: define void @inc_tagged(ptr %0)
  // CHECK-NEXT: %[[RAW:.*]] = ptrtoint ptr %0 to i64
  // CHECK-NEXT: %[[LOW:.*]] = and i64 %[[RAW]], 1
  // CHECK-NEXT: %[[BOX:.*]] = icmp eq i64 %[[LOW]], 0
  // CHECK-NEXT: br i1 %[[BOX]], label %[[INC:.*]], label %[[DONE:.*]]
  // CHECK: [[INC]]:
  // CHECK-NEXT: %[[ADDR:.*]] = call ptr @llvm.ptrmask.p0.i64(ptr %0, i64 281474976710655)
  // CHECK-NEXT: %[[OLD:.*]] = load i32, ptr %[[ADDR]]
  // CHECK-NEXT: %[[NEW:.*]] = add i32 %[[OLD]], 1
  // CHECK-NEXT: store i32 %[[NEW]], ptr %[[ADDR]]
  // CHECK: br label %[[DONE]]
  // CHECK: [[DONE]]:
  // CHECK-NEXT: ret void
  func.func @inc_tagged(%w: !tagged) {
    reussir.rc.inc(%w : !tagged)
    return
  }

  // CHECK-LABEL: define void @inc_tagged_delta(ptr %0, i64 %1)
  // CHECK: br i1 %{{.*}}, label %[[INC:.*]], label %[[DONE:.*]]
  // CHECK: [[INC]]:
  // CHECK-NEXT: %[[ADDR:.*]] = call ptr @llvm.ptrmask.p0.i64(ptr %0, i64 281474976710655)
  // CHECK: %[[OLD:.*]] = load i32, ptr %[[ADDR]]
  // CHECK: store i32 %{{.*}}, ptr %[[ADDR]]
  func.func @inc_tagged_delta(%w: !tagged, %n: index) {
    reussir.rc.inc(%w : !tagged) by %n
    return
  }

  // CHECK-LABEL: define void @dec_tagged(ptr %0)
  // CHECK-NEXT: %[[RAW:.*]] = ptrtoint ptr %0 to i64
  // CHECK-NEXT: %[[LOW:.*]] = and i64 %[[RAW]], 1
  // CHECK-NEXT: %[[BOX:.*]] = icmp eq i64 %[[LOW]], 0
  // CHECK-NEXT: br i1 %[[BOX]], label %[[REL:.*]], label %[[DONE:.*]]
  // CHECK: [[REL]]:
  // CHECK-NEXT: call void @word_drop(ptr %0)
  // CHECK-NEXT: br label %[[DONE]]
  // CHECK: [[DONE]]:
  // CHECK-NEXT: ret void
  func.func @dec_tagged(%w: !tagged) {
    reussir.rc.dec(%w : !tagged)
    return
  }

  // CHECK-LABEL: define void @inc_tagged_atomic(ptr %0)
  // CHECK: br i1 %{{.*}}, label %[[INC:.*]], label %[[DONE:.*]]
  // CHECK: [[INC]]:
  // CHECK-NEXT: %[[ADDR:.*]] = call ptr @llvm.ptrmask.p0.i64(ptr %0, i64 281474976710655)
  // CHECK-NEXT: atomicrmw add ptr %[[ADDR]], i32 1 monotonic
  func.func @inc_tagged_atomic(%w: !taggedA) {
    reussir.rc.inc(%w : !taggedA)
    return
  }

  // CHECK-LABEL: define void @dec_tagged_atomic(ptr %0)
  // CHECK: call void @word_drop_atomic(ptr %0)
  func.func @dec_tagged_atomic(%w: !taggedA) {
    reussir.rc.dec(%w : !taggedA)
    return
  }

  // CHECK-LABEL: define void @inc_plain(ptr %0)
  // CHECK-NOT: ptrtoint
  // CHECK-NOT: ptrmask
  // CHECK: load i32, ptr %0
  // CHECK: store i32
  func.func @inc_plain(%b: !plain) {
    reussir.rc.inc(%b : !plain)
    return
  }

  // CHECK-LABEL: define void @dec_plain(ptr %0)
  // CHECK-NEXT: call void @box_drop(ptr %0)
  // CHECK-NEXT: ret void
  func.func @dec_plain(%b: !plain) {
    reussir.rc.dec(%b : !plain)
    return
  }
}

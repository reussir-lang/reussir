// RUN: %reussir-opt %s --reussir-convert-to-std | %FileCheck %s --check-prefix=KEEP
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-lowering-basic-ops --convert-to-llvm --reconcile-unrealized-casts | %reussir-translate --mlir-to-llvmir | %FileCheck %s --check-prefix=LLVM

module {
  // KEEP-LABEL: func.func @integer
  // KEEP: reussir.array.fill_pattern
  // LLVM-LABEL: define void @integer
  // LLVM: call void @llvm.experimental.memset.pattern.p0.i32.i64(ptr %{{.*}}, i32 %{{.*}}, i64 %{{.*}}, i1 false)
  func.func @integer(%dst: !reussir.ref<!reussir.array<? x i32>>, %value: i32, %count: index) {
    reussir.array.fill_pattern(%dst : !reussir.ref<!reussir.array<? x i32>>) init(%value : i32) count(%count)
    return
  }

  // KEEP-LABEL: func.func @pointer
  // KEEP: reussir.array.fill_pattern
  // LLVM-LABEL: define void @pointer
  // LLVM: call void @llvm.experimental.memset.pattern.p0.p0.i64(ptr %{{.*}}, ptr %{{.*}}, i64 %{{.*}}, i1 false)
  func.func @pointer(%dst: !reussir.ref<!reussir.array<4 x !reussir.rc<i32>>>, %value: !reussir.rc<i32>, %count: index) {
    reussir.array.fill_pattern(%dst : !reussir.ref<!reussir.array<4 x !reussir.rc<i32>>>) init(%value : !reussir.rc<i32>) count(%count)
    return
  }
}

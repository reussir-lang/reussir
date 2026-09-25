// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-lowering-basic-ops --convert-to-llvm --reconcile-unrealized-casts | %reussir-translate --mlir-to-llvmir | %FileCheck %s

module {
  // CHECK-LABEL: define void @normal
  // CHECK: trunc i64 %{{.*}} to i32
  // CHECK: add i32
  // CHECK: store i32
  func.func @normal(%rc: !reussir.rc<i32>, %n: index) {
    reussir.rc.inc(%rc : !reussir.rc<i32>) by %n
    return
  }
  // CHECK-LABEL: define void @atomic
  // CHECK: %[[DELTA:.*]] = trunc i64 %{{.*}} to i32
  // CHECK: atomicrmw add ptr %{{.*}}, i32 %[[DELTA]] monotonic
  func.func @atomic(%rc: !reussir.rc<i32 atomic>, %n: index) {
    reussir.rc.inc(%rc : !reussir.rc<i32 atomic>) by %n
    return
  }
  // CHECK-LABEL: define void @rigid
  // CHECK: call void @__reussir_acquire_rigid_object(ptr %{{.*}}, i64 %{{.*}})
  func.func @rigid(%rc: !reussir.rc<i32 rigid>, %n: index) {
    reussir.rc.inc(%rc : !reussir.rc<i32 rigid>) by %n
    return
  }
}

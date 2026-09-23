// RUN: %reussir-opt %s --reussir-token-instantiation --reussir-convert-to-std="expand-arrays=false" | %FileCheck %s --check-prefix=DEFER
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --sccp --canonicalize --loop-invariant-code-motion --reussir-convert-to-std --reussir-acquire-drop-expansion | %FileCheck %s

module {
  // DEFER-LABEL: func.func @fixed
  // DEFER: reussir.array.create
  // DEFER: reussir.scf.yield
  // DEFER-NOT: reussir.array.instantiate
  // DEFER: return
  // CHECK-LABEL: func.func @fixed
  // CHECK: reussir.array.instantiate
  // CHECK: reussir.array.fill_pattern
  // CHECK-NOT: scf.for
  // CHECK: return
  func.func @fixed(%value: f32) -> !reussir.rc<!reussir.array<2 x 3 x f32>> {
    %array = reussir.array.create extents() : !reussir.rc<!reussir.array<2 x 3 x f32>> body {
      ^bb0(%array_i0: index, %array_i1: index):
        reussir.scf.yield %value : f32
    }
    return %array : !reussir.rc<!reussir.array<2 x 3 x f32>>
  }

  // CHECK-LABEL: func.func @managed
  // CHECK: reussir.array.instantiate
  // CHECK: %[[COUNT:.*]] = arith.muli %arg0, %{{.*}} : index
  // CHECK: scf.if
  // CHECK: reussir.rc.inc{{.*}} by %[[COUNT]]
  // CHECK: reussir.array.fill_pattern{{.*}}count(%[[COUNT]])
  // CHECK-NOT: scf.for
  // CHECK: return
  func.func @managed(%n: index, %value: !reussir.rc<i32>) -> !reussir.rc<!reussir.array<? x 4 x !reussir.rc<i32>>> {
    %array = reussir.array.create extents(%n) : !reussir.rc<!reussir.array<? x 4 x !reussir.rc<i32>>> body {
      ^bb0(%array_i0: index, %array_i1: index):
        reussir.rc.inc(%value : !reussir.rc<i32>)
        reussir.scf.yield %value : !reussir.rc<i32>
    }
    return %array : !reussir.rc<!reussir.array<? x 4 x !reussir.rc<i32>>>
  }

  // CHECK-LABEL: func.func @tabulate
  // CHECK: scf.for
  // CHECK: scf.for
  // CHECK: arith.addi
  // CHECK: memref.store
  // CHECK: return
  func.func @tabulate() -> !reussir.rc<!reussir.array<2 x 3 x index>> {
    %array = reussir.array.create extents() : !reussir.rc<!reussir.array<2 x 3 x index>> body {
      ^bb0(%i: index, %j: index):
        %sum = arith.addi %i, %j : index
        reussir.scf.yield %sum : index
    }
    return %array : !reussir.rc<!reussir.array<2 x 3 x index>>
  }
  // CHECK-LABEL: func.func @folded_constant
  // CHECK: reussir.array.fill_pattern
  // CHECK-NOT: scf.for
  // CHECK: return
  func.func @folded_constant() -> !reussir.rc<!reussir.array<8 x index>> {
    %array = reussir.array.create extents() : !reussir.rc<!reussir.array<8 x index>> body {
      ^bb0(%i: index):
        %zero = arith.constant 0 : index
        %seven = arith.constant 7 : index
        %product = arith.muli %i, %zero : index
        %value = arith.addi %product, %seven : index
        reussir.scf.yield %value : index
    }
    return %array : !reussir.rc<!reussir.array<8 x index>>
  }

  func.func private @observe(index)
  // CHECK-LABEL: func.func @effectful
  // CHECK: scf.for
  // CHECK: call @observe
  // CHECK: memref.store
  // CHECK-NOT: reussir.array.fill_pattern
  // CHECK: return
  func.func @effectful(%value: i32) -> !reussir.rc<!reussir.array<8 x i32>> {
    %array = reussir.array.create extents() : !reussir.rc<!reussir.array<8 x i32>> body {
      ^bb0(%i: index):
        func.call @observe(%i) : (index) -> ()
        reussir.scf.yield %value : i32
    }
    return %array : !reussir.rc<!reussir.array<8 x i32>>
  }
}

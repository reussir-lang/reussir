// RUN: %reussir-opt %s --cse --canonicalize | %FileCheck %s
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,cse,canonicalize,reussir-convert-to-std,convert-scf-to-cf,convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir | %llc -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe

!variant = !reussir.record<variant "Choice" {i32, i64}>
!ref = !reussir.ref<!variant field>
module {
  // The tag is read by dispatch itself, even when both regions are pure.
  // CHECK-LABEL: func.func @main
  // CHECK: reussir.record.dispatch
  // CHECK: reussir.ref.store
  // CHECK: reussir.record.dispatch
  func.func @main() -> i32 {
    %size = arith.constant 32 : i64
    %slot = llvm.alloca %size x i8 {alignment = 8 : i64} : (i64) -> !llvm.ptr
    %ref = builtin.unrealized_conversion_cast %slot : !llvm.ptr to !ref
    %zero = arith.constant 0 : i32
    %one = arith.constant 1 : i32
    %wide = arith.constant 0 : i64
    %a = reussir.record.variant [0] (%zero : i32) : !variant
    %b = reussir.record.variant [1] (%wide : i64) : !variant
    reussir.ref.store(%ref : !ref)(%a : !variant)
    %first = reussir.record.dispatch(%ref : !ref) -> i32 {
      [0] -> {
      ^bb0(%v: !reussir.ref<i32 field>):
        reussir.scf.yield %zero : i32
      }
      [1] -> {
      ^bb0(%v: !reussir.ref<i64 field>):
        reussir.scf.yield %one : i32
      }
    }
    reussir.ref.store(%ref : !ref)(%b : !variant)
    %second = reussir.record.dispatch(%ref : !ref) -> i32 {
      [0] -> {
      ^bb0(%v: !reussir.ref<i32 field>):
        reussir.scf.yield %zero : i32
      }
      [1] -> {
      ^bb0(%v: !reussir.ref<i64 field>):
        reussir.scf.yield %one : i32
      }
    }
    %difference = arith.subi %second, %first : i32
    %ok = arith.cmpi eq, %difference, %one : i32
    cf.assert %ok, "CSE merged tag reads across a variant store"
    %counter = memref.alloca() : memref<i32>
    memref.store %zero, %counter[] : memref<i32>
    func.call @nested_effects(%ref, %counter) : (!ref, memref<i32>) -> ()
    func.call @unknown_effects(%ref, %counter) : (!ref, memref<i32>) -> ()
    %count = memref.load %counter[] : memref<i32>
    %four = arith.constant 4 : i32
    %twice = arith.cmpi eq, %count, %four : i32
    cf.assert %twice, "dispatch must retain recursive effects"
    return %zero : i32
  }

  // Adding the outer read interface must not hide writes in the body, even
  // when the dispatch has no result and the tag does not change.
  // CHECK-LABEL: func.func @nested_effects
  // CHECK: reussir.record.dispatch
  // CHECK: memref.store
  // CHECK: reussir.record.dispatch
  // CHECK: memref.store
  func.func @nested_effects(%ref: !ref, %counter: memref<i32>) {
    %one = arith.constant 1 : i32
    reussir.record.dispatch(%ref : !ref) {
      [0, 1] -> {
        %old = memref.load %counter[] : memref<i32>
        %next = arith.addi %old, %one : i32
        memref.store %next, %counter[] : memref<i32>
        reussir.scf.yield
      }
    }
    reussir.record.dispatch(%ref : !ref) {
      [0, 1] -> {
        %old = memref.load %counter[] : memref<i32>
        %next = arith.addi %old, %one : i32
        memref.store %next, %counter[] : memref<i32>
        reussir.scf.yield
      }
    }
    return
  }

  func.func @bump(%counter: memref<i32>) {
    %one = arith.constant 1 : i32
    %old = memref.load %counter[] : memref<i32>
    %next = arith.addi %old, %one : i32
    memref.store %next, %counter[] : memref<i32>
    return
  }

  // Calls without an effect summary must also prevent merging dispatches.
  // CHECK-LABEL: func.func @unknown_effects
  // CHECK: reussir.record.dispatch
  // CHECK: func.call @bump
  // CHECK: reussir.record.dispatch
  // CHECK: func.call @bump
  func.func @unknown_effects(%ref: !ref, %counter: memref<i32>) {
    reussir.record.dispatch(%ref : !ref) {
      [0, 1] -> {
        func.call @bump(%counter) : (memref<i32>) -> ()
        reussir.scf.yield
      }
    }
    reussir.record.dispatch(%ref : !ref) {
      [0, 1] -> {
        func.call @bump(%counter) : (memref<i32>) -> ()
        reussir.scf.yield
      }
    }
    return
  }

  // With no intervening mutation, read-only dispatches still CSE: their
  // difference folds away, and the now-unused reads can be removed.
  // CHECK-LABEL: func.func @read_twice
  // CHECK: %[[ZERO:.*]] = arith.constant 0 : i32
  // CHECK-NOT: reussir.record.dispatch
  // CHECK: return %[[ZERO]] : i32
  func.func @read_twice(%ref: !ref) -> i32 {
    %zero = arith.constant 0 : i32
    %one = arith.constant 1 : i32
    %first = reussir.record.dispatch(%ref : !ref) -> i32 {
      [0] -> {
      ^bb0(%v: !reussir.ref<i32 field>):
        reussir.scf.yield %zero : i32
      }
      [1] -> {
      ^bb0(%v: !reussir.ref<i64 field>):
        reussir.scf.yield %one : i32
      }
    }
    %second = reussir.record.dispatch(%ref : !ref) -> i32 {
      [0] -> {
      ^bb0(%v: !reussir.ref<i32 field>):
        reussir.scf.yield %zero : i32
      }
      [1] -> {
      ^bb0(%v: !reussir.ref<i64 field>):
        reussir.scf.yield %one : i32
      }
    }
    %difference = arith.subi %second, %first : i32
    return %difference : i32
  }
}

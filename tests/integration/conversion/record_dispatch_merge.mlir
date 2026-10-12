// Canonicalization merges the regions of a `reussir.record.dispatch` that do
// not use their argument and hold the same operations: the frontend gives
// each variant its own region, so a wildcard arm arrives as one copy per
// variant it covers. Then the merged dispatch must still pick the right code
// for every tag.

// RUN: %reussir-opt %s --canonicalize | %FileCheck %s
// RUN: %reussir-opt %s --canonicalize | %reussir-opt --canonicalize | %FileCheck %s
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,canonicalize,reussir-convert-to-std,convert-scf-to-cf,convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir | %llc -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe

!variant = !reussir.record<variant "Wide" {i32, i64, i32, i64, i32}>
!ref = !reussir.ref<!variant field>
#wild = loc("wild.rr":7:14)
#other = loc("wild.rr":8:14)
module {
  func.func @bump(%counter: memref<i32>) {
    %one = arith.constant 1 : i32
    %old = memref.load %counter[] : memref<i32>
    %next = arith.addi %old, %one : i32
    memref.store %next, %counter[] : memref<i32>
    return
  }

  func.func @bump_twice(%counter: memref<i32>) {
    func.call @bump(%counter) : (memref<i32>) -> ()
    func.call @bump(%counter) : (memref<i32>) -> ()
    return
  }

  // Region 0 reads its payload and stays. Regions 1, 2 and 4 are copies of
  // one wildcard arm (same operations, same locations): one region over
  // their tags, without the argument. Region 3 calls another function.
  // CHECK-LABEL: func.func @classify
  // CHECK: reussir.record.dispatch
  // CHECK-NEXT: [0] -> {
  // CHECK-NEXT: ^bb0
  // CHECK: [1, 2, 4] -> {
  // CHECK-NOT: ^bb0
  // CHECK: func.call @bump(
  // CHECK: [3] -> {
  // CHECK-NEXT: ^bb0
  // CHECK-NEXT: func.call @bump_twice
  func.func @classify(%ref: !ref, %counter: memref<i32>, %flag: i1) -> i32 {
    %seven = arith.constant 7 : i32
    %result = reussir.record.dispatch(%ref : !ref) -> i32 {
      [0] -> {
      ^bb0(%v: !reussir.ref<i32 field>):
        %x = reussir.ref.load(%v : !reussir.ref<i32 field>) : i32
        reussir.scf.yield %x : i32
      }
      [1] -> {
      ^bb0(%v: !reussir.ref<i64 field>):
        %r = scf.if %flag -> (i32) {
          func.call @bump(%counter) : (memref<i32>) -> () loc(#wild)
          scf.yield %seven : i32 loc(#wild)
        } else {
          %t = arith.addi %seven, %seven : i32 loc(#wild)
          scf.yield %t : i32 loc(#wild)
        } loc(#wild)
        reussir.scf.yield %r : i32 loc(#wild)
      }
      [2] -> {
      ^bb0(%v: !reussir.ref<i32 field>):
        %r = scf.if %flag -> (i32) {
          func.call @bump(%counter) : (memref<i32>) -> () loc(#wild)
          scf.yield %seven : i32 loc(#wild)
        } else {
          %t = arith.addi %seven, %seven : i32 loc(#wild)
          scf.yield %t : i32 loc(#wild)
        } loc(#wild)
        reussir.scf.yield %r : i32 loc(#wild)
      }
      [3] -> {
      ^bb0(%v: !reussir.ref<i64 field>):
        func.call @bump_twice(%counter) : (memref<i32>) -> () loc(#wild)
        reussir.scf.yield %seven : i32 loc(#wild)
      }
      [4] -> {
      ^bb0(%v: !reussir.ref<i32 field>):
        %r = scf.if %flag -> (i32) {
          func.call @bump(%counter) : (memref<i32>) -> () loc(#wild)
          scf.yield %seven : i32 loc(#wild)
        } else {
          %t = arith.addi %seven, %seven : i32 loc(#wild)
          scf.yield %t : i32 loc(#wild)
        } loc(#wild)
        reussir.scf.yield %r : i32 loc(#wild)
      }
    }
    return %result : i32
  }

  // The same operations at different source locations are different arms:
  // merging them would lose the location of one.
  // CHECK-LABEL: func.func @distinct_locations
  // CHECK: [0, 1] -> {
  // CHECK: [2] -> {
  // CHECK: [3, 4] -> {
  func.func @distinct_locations(%ref: !ref, %counter: memref<i32>) {
    reussir.record.dispatch(%ref : !ref) {
      [0, 1] -> {
        func.call @bump(%counter) : (memref<i32>) -> () loc(#wild)
        reussir.scf.yield loc(#wild)
      }
      [2] -> {
      ^bb0(%v: !reussir.ref<i32 field>):
        func.call @bump(%counter) : (memref<i32>) -> () loc(#other)
        reussir.scf.yield loc(#other)
      }
      [3, 4] -> {
        func.call @bump_twice(%counter) : (memref<i32>) -> () loc(#wild)
        reussir.scf.yield loc(#wild)
      }
    }
    return
  }

  // A region that already covers several tags absorbs a copy over another
  // tag; when all tags end up in one region, the dispatch keeps it.
  // CHECK-LABEL: func.func @all_tags
  // CHECK: reussir.record.dispatch
  // CHECK-NEXT: [0, 1, 2, 3, 4] -> {
  // CHECK-NEXT: func.call @bump
  // CHECK-NEXT: reussir.scf.yield
  // CHECK-NEXT: }
  // CHECK-NEXT: }
  func.func @all_tags(%ref: !ref, %counter: memref<i32>) {
    reussir.record.dispatch(%ref : !ref) {
      [0, 3] -> {
        func.call @bump(%counter) : (memref<i32>) -> () loc(#wild)
        reussir.scf.yield loc(#wild)
      }
      [1] -> {
      ^bb0(%v: !reussir.ref<i64 field>):
        func.call @bump(%counter) : (memref<i32>) -> () loc(#wild)
        reussir.scf.yield loc(#wild)
      }
      [2, 4] -> {
        func.call @bump(%counter) : (memref<i32>) -> () loc(#wild)
        reussir.scf.yield loc(#wild)
      }
    }
    return
  }

  // Identical structure is insufficient: the captured SSA values must match.
  // The first two arms have equal structural hashes but use different values.
  // CHECK-LABEL: func.func @distinct_captures
  // CHECK-SAME: {{.*}}, %[[LEFT:[^:]+]]: memref<i32>, %[[RIGHT:[^:]+]]: memref<i32>
  // CHECK: [0, 2] -> {
  // CHECK-NEXT: func.call @bump(%[[LEFT]])
  // CHECK: [1, 3, 4] -> {
  // CHECK-NEXT: func.call @bump(%[[RIGHT]])
  func.func @distinct_captures(%ref: !ref, %left: memref<i32>, %right: memref<i32>) {
    reussir.record.dispatch(%ref : !ref) {
      [0] -> {
      ^bb0(%v: !reussir.ref<i32 field>):
        func.call @bump(%left) : (memref<i32>) -> () loc(#wild)
        reussir.scf.yield loc(#wild)
      }
      [1, 3, 4] -> {
        func.call @bump(%right) : (memref<i32>) -> () loc(#wild)
        reussir.scf.yield loc(#wild)
      }
      [2] -> {
      ^bb0(%v: !reussir.ref<i32 field>):
        func.call @bump(%left) : (memref<i32>) -> () loc(#wild)
        reussir.scf.yield loc(#wild)
      }
    }
    return
  }

  func.func @check(%ref: !ref, %counter: memref<i32>, %flag: i1,
                   %value: !variant, %expected: i32, %bumps: i32) {
    %zero = arith.constant 0 : i32
    memref.store %zero, %counter[] : memref<i32>
    reussir.ref.store(%ref : !ref)(%value : !variant)
    %got = func.call @classify(%ref, %counter, %flag)
        : (!ref, memref<i32>, i1) -> i32
    %same = arith.cmpi eq, %got, %expected : i32
    cf.assert %same, "merged dispatch picked the wrong region"
    %count = memref.load %counter[] : memref<i32>
    %counted = arith.cmpi eq, %count, %bumps : i32
    cf.assert %counted, "merged dispatch ran the wrong code"
    return
  }

  func.func @main() -> i32 {
    %size = arith.constant 32 : i64
    %slot = llvm.alloca %size x i8 {alignment = 8 : i64} : (i64) -> !llvm.ptr
    %ref = builtin.unrealized_conversion_cast %slot : !llvm.ptr to !ref
    %counter = memref.alloca() : memref<i32>
    %true = arith.constant true
    %false = arith.constant false
    %zero = arith.constant 0 : i32
    %one = arith.constant 1 : i32
    %two = arith.constant 2 : i32
    %seven = arith.constant 7 : i32
    %fourteen = arith.constant 14 : i32
    %forty_two = arith.constant 42 : i32
    %wide = arith.constant 5 : i64
    %v0 = reussir.record.variant [0] (%forty_two : i32) : !variant
    %v1 = reussir.record.variant [1] (%wide : i64) : !variant
    %v2 = reussir.record.variant [2] (%forty_two : i32) : !variant
    %v3 = reussir.record.variant [3] (%wide : i64) : !variant
    %v4 = reussir.record.variant [4] (%forty_two : i32) : !variant
    func.call @check(%ref, %counter, %true, %v0, %forty_two, %zero)
        : (!ref, memref<i32>, i1, !variant, i32, i32) -> ()
    func.call @check(%ref, %counter, %true, %v1, %seven, %one)
        : (!ref, memref<i32>, i1, !variant, i32, i32) -> ()
    func.call @check(%ref, %counter, %false, %v2, %fourteen, %zero)
        : (!ref, memref<i32>, i1, !variant, i32, i32) -> ()
    func.call @check(%ref, %counter, %true, %v3, %seven, %two)
        : (!ref, memref<i32>, i1, !variant, i32, i32) -> ()
    func.call @check(%ref, %counter, %true, %v4, %seven, %one)
        : (!ref, memref<i32>, i1, !variant, i32, i32) -> ()
    // The slot holds variant 4 now.
    memref.store %zero, %counter[] : memref<i32>
    func.call @distinct_locations(%ref, %counter) : (!ref, memref<i32>) -> ()
    %d = memref.load %counter[] : memref<i32>
    %d_ok = arith.cmpi eq, %d, %two : i32
    cf.assert %d_ok, "distinct_locations ran the wrong code"
    memref.store %zero, %counter[] : memref<i32>
    func.call @all_tags(%ref, %counter) : (!ref, memref<i32>) -> ()
    %a = memref.load %counter[] : memref<i32>
    %a_ok = arith.cmpi eq, %a, %one : i32
    cf.assert %a_ok, "all_tags ran the wrong code"
    return %zero : i32
  }
}

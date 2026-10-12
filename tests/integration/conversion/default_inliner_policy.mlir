// RUN: %reussir-opt %s --split-input-file --reussir-default-inliner | %FileCheck %s --check-prefix=INLINE --implicit-check-not=reussir.inliner_home
// RUN: %reussir-opt %s --split-input-file --reussir-default-inliner='max-callee-ops=0' | %FileCheck %s --check-prefix=CAPPED --implicit-check-not=reussir.inliner_home

// Acyclic helpers continue to inline. Side effects must not be duplicated.
module {
  func.func private @observe(i32)
  func.func private @leaf(%x: i32) -> i32 {
    func.call @observe(%x) : (i32) -> ()
    %one = arith.constant 1 : i32
    %r = arith.addi %x, %one : i32
    return %r : i32
  }
  func.func private @middle(%x: i32) -> i32 {
    %r = func.call @leaf(%x) : (i32) -> i32
    return %r : i32
  }
  // INLINE-LABEL: func.func @entry
  // INLINE: call @observe
  // INLINE-NOT: call
  // INLINE: arith.addi
  // INLINE-NOT: call
  // INLINE: return
  // CAPPED-LABEL: func.func @entry
  // CAPPED: call @middle
  // CAPPED: return
  func.func @entry(%x: i32) -> i32 {
    %r = func.call @middle(%x) : (i32) -> i32
    return %r : i32
  }
}

// -----

// A direct recursive call stays valid, and neither accepted nor rejected
// calls retain the pass's temporary attributes.
module {
  // INLINE-LABEL: func.func @self
  // INLINE: call @self
  // INLINE: return
  // CAPPED-LABEL: func.func @self
  // CAPPED: call @self
  // CAPPED: return
  func.func @self(%x: i32) -> i32 {
    %zero = arith.constant 0 : i32
    %one = arith.constant 1 : i32
    %done = arith.cmpi eq, %x, %zero : i32
    %r = scf.if %done -> (i32) {
      scf.yield %zero : i32
    } else {
      %next = arith.subi %x, %one : i32
      %r = func.call @self(%next) : (i32) -> i32
      scf.yield %r : i32
    }
    return %r : i32
  }
}

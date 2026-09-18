// RUN: %reussir-opt %s --loop-invariant-code-motion --cse | %FileCheck %s

module {
  reussir.str.global @text = "hello"
  func.func private @consume(i8, index, i1, i1, i1, i32)
  func.func private @consume_unchecked(i8, i1, i32)
  func.func private @effect()

  // Immutable string contents cannot be changed by even an unknown call.
  // All checked operations can move before a possibly empty loop.
  // CHECK-LABEL: func.func @checked
  // CHECK-DAG: reussir.str.byte_at
  // CHECK-DAG: reussir.str.select
  // CHECK-DAG: reussir.str.startswith
  // CHECK-DAG: reussir.str.equal
  // CHECK-DAG: reussir.str.compare
  // CHECK: scf.for
  // CHECK: call @effect
  // CHECK-NOT: reussir.str.
  // CHECK: return
  func.func @checked(%a: !reussir.str<local>, %b: !reussir.str<local>, %index: index, %n: index) {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    scf.for %i = %c0 to %n step %c1 {
      func.call @effect() : () -> ()
      %byte = reussir.str.byte_at(%a : !reussir.str<local>)[%index : index] : i8
      %selected, %found = reussir.str.select(%a) ["hi"] : (!reussir.str<local>) -> (index, i1)
      %prefix = reussir.str.startswith(%a : !reussir.str<local>) "hi" : i1
      %equal = reussir.str.equal(%a : !reussir.str<local>, %b : !reussir.str<local>) : i1
      %cmp = reussir.str.compare(%a : !reussir.str<local>, %b : !reussir.str<local>) : i32
      func.call @consume(%byte, %selected, %found, %prefix, %equal, %cmp) : (i8, index, i1, i1, i1, i32) -> ()
    }
    return
  }

  // Bounds proofs allow all unchecked reads of a literal slice to hoist too.
  // CHECK-LABEL: func.func @proven_bounds
  // CHECK-DAG: reussir.str.unsafe_byte_at
  // CHECK-DAG: reussir.str.unsafe_startswith
  // CHECK-DAG: reussir.str.unsafe_memcmp
  // CHECK: scf.for
  // CHECK-NOT: reussir.str.
  // CHECK: return
  func.func @proven_bounds(%n: index) {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %c2 = arith.constant 2 : index
    %c3 = arith.constant 3 : index
    %global = reussir.str.literal @text : !reussir.str<global>
    %local = reussir.str.cast(%global : !reussir.str<global>) : !reussir.str<local>
    %tail = reussir.str.slice(%local : !reussir.str<local>)[%c2] : !reussir.str<local>
    scf.for %i = %c0 to %n step %c1 {
      %byte = reussir.str.unsafe_byte_at(%tail : !reussir.str<local>)[%c2 : index] : i8
      %prefix = reussir.str.unsafe_startswith(%tail : !reussir.str<local>) "llo" : i1
      %cmp = reussir.str.unsafe_memcmp(%tail : !reussir.str<local>, %tail : !reussir.str<local>)[%c3] : i32
      func.call @consume_unchecked(%byte, %prefix, %cmp) : (i8, i1, i32) -> ()
    }
    return
  }

  // Unknown bounds still require the original execution guard.
  // CHECK-LABEL: func.func @unknown_bounds
  // CHECK: scf.for
  // CHECK: reussir.str.unsafe_byte_at
  // CHECK: reussir.str.unsafe_startswith
  // CHECK: reussir.str.unsafe_memcmp
  func.func @unknown_bounds(%a: !reussir.str<local>, %index: index, %n: index) {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    scf.for %i = %c0 to %n step %c1 {
      %byte = reussir.str.unsafe_byte_at(%a : !reussir.str<local>)[%index : index] : i8
      %prefix = reussir.str.unsafe_startswith(%a : !reussir.str<local>) "hi" : i1
      %cmp = reussir.str.unsafe_memcmp(%a : !reussir.str<local>, %a : !reussir.str<local>)[%index] : i32
      func.call @consume_unchecked(%byte, %prefix, %cmp) : (i8, i1, i32) -> ()
    }
    return
  }

  // CSE may reuse a content comparison across arbitrary memory effects.
  // CHECK-LABEL: func.func @reuse_comparison
  // CHECK: %[[EQ:.*]] = reussir.str.equal
  // CHECK: call @effect
  // CHECK-NOT: reussir.str.equal
  // CHECK: return %[[EQ]], %[[EQ]]
  func.func @reuse_comparison(%a: !reussir.str<local>, %b: !reussir.str<local>) -> (i1, i1) {
    %first = reussir.str.equal(%a : !reussir.str<local>, %b : !reussir.str<local>) : i1
    func.call @effect() : () -> ()
    %second = reussir.str.equal(%a : !reussir.str<local>, %b : !reussir.str<local>) : i1
    return %first, %second : i1, i1
  }
}

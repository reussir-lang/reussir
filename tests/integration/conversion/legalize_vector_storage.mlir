// RUN: %reussir-opt %s --split-input-file --verify-diagnostics \
// RUN:   --reussir-legalize-vector-storage | %FileCheck %s

// A function without any vector, whose blocks are listed last first. Block k
// gets two arguments that are equal only once block k-1 has lost its own, so
// the greedy driver's region simplification clears one block per iteration
// and is not done after ten. MLIR's `arm-sve-legalize-vector-storage` fails
// on it; the patterns alone leave it as it is.

// CHECK-LABEL: func.func @chain(
// CHECK: cf.br ^[[FIRST:.+]](%{{.+}}, %{{.+}} : i64, i64)
// CHECK: arith.addi
// CHECK: cf.cond_br
func.func @chain(%p: i1, %c: i64) -> i64 {
  cf.br ^d0(%c, %c : i64, i64)
^d10(%v10: i64, %w10: i64):
  %r = arith.addi %v10, %w10 : i64
  return %r : i64
^d9(%v9: i64, %w9: i64):
  cf.cond_br %p, ^d10(%v9, %w9 : i64, i64), ^d10(%w9, %v9 : i64, i64)
^d8(%v8: i64, %w8: i64):
  cf.cond_br %p, ^d9(%v8, %w8 : i64, i64), ^d9(%w8, %v8 : i64, i64)
^d7(%v7: i64, %w7: i64):
  cf.cond_br %p, ^d8(%v7, %w7 : i64, i64), ^d8(%w7, %v7 : i64, i64)
^d6(%v6: i64, %w6: i64):
  cf.cond_br %p, ^d7(%v6, %w6 : i64, i64), ^d7(%w6, %v6 : i64, i64)
^d5(%v5: i64, %w5: i64):
  cf.cond_br %p, ^d6(%v5, %w5 : i64, i64), ^d6(%w5, %v5 : i64, i64)
^d4(%v4: i64, %w4: i64):
  cf.cond_br %p, ^d5(%v4, %w4 : i64, i64), ^d5(%w4, %v4 : i64, i64)
^d3(%v3: i64, %w3: i64):
  cf.cond_br %p, ^d4(%v3, %w3 : i64, i64), ^d4(%w3, %v3 : i64, i64)
^d2(%v2: i64, %w2: i64):
  cf.cond_br %p, ^d3(%v2, %w2 : i64, i64), ^d3(%w2, %v2 : i64, i64)
^d1(%v1: i64, %w1: i64):
  cf.cond_br %p, ^d2(%v1, %w1 : i64, i64), ^d2(%w1, %v1 : i64, i64)
^d0(%v0: i64, %w0: i64):
  cf.cond_br %p, ^d1(%v0, %w0 : i64, i64), ^d1(%w0, %v0 : i64, i64)
}

// -----

// A predicate slot that a call takes cannot change its type.
func.func private @use(memref<vector<[4]xi1>>)
func.func @escape(%m: vector<[4]xi1>) -> vector<[4]xi1> {
  // expected-error@+1 {{the storage of a scalable vector is used by an operation that cannot be legalized}}
  %a = memref.alloca() : memref<vector<[4]xi1>>
  memref.store %m, %a[] : memref<vector<[4]xi1>>
  call @use(%a) : (memref<vector<[4]xi1>>) -> ()
  %r = memref.load %a[] : memref<vector<[4]xi1>>
  return %r : vector<[4]xi1>
}

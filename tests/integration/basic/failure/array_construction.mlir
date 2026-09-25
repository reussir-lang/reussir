// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

func.func @fixed_rc_create(%value: !reussir.array<4 x i32>) {
  // expected-error @+1 {{arrays must be created with array.create}}
  %array = reussir.rc.create value(%value : !reussir.array<4 x i32>) : !reussir.rc<!reussir.array<4 x i32>>
  return
}
// -----
func.func @multiple_blocks(%value: i32) {
  // expected-error @+1 {{region with at most 1 blocks}}
  %array = reussir.array.create extents() : !reussir.rc<!reussir.array<4 x i32>> body {
    ^bb0(%i: index):
      cf.br ^bb1
    ^bb1:
      reussir.scf.yield %value : i32
  }
  return
}
// -----
func.func @uninitialized_missing_extent() {
  // expected-error @+1 {{expects 1 extent operand(s), got 0}}
  %array = reussir.array.create extents() : !reussir.rc<!reussir.array<? x i32>>
  return
}
// -----
func.func @uninitialized_wrong_token(%token: !reussir.token<align: 4, size: 4>) {
  // expected-error @+1 {{expected token type}}
  %array = reussir.array.create extents() token(%token : !reussir.token<align: 4, size: 4>) : !reussir.rc<!reussir.array<4 x i32>>
  return
}
// -----
func.func @wrong_indices() {
  // expected-error @+1 {{initializer body requires one index per dimension}}
  %array = reussir.array.create extents() : !reussir.rc<!reussir.array<2 x 3 x i32>> body {
    ^bb0(%i: index):
      %zero = arith.constant 0 : i32
      reussir.scf.yield %zero : i32
  }
  return
}
// -----
func.func @wrong_pattern(%dst: !reussir.ref<!reussir.array<4 x i32>>, %value: i64, %n: index) {
  // expected-error @+1 {{requires an array reference and matching element type}}
  reussir.array.fill_pattern(%dst : !reussir.ref<!reussir.array<4 x i32>>) init(%value : i64) count(%n)
  return
}
// -----
func.func @negative_fill(%dst: !reussir.ref<!reussir.array<4 x i32>>, %value: i32) {
  %n = arith.constant -1 : index
  // expected-error @+1 {{fill count must be non-negative}}
  reussir.array.fill_pattern(%dst : !reussir.ref<!reussir.array<4 x i32>>) init(%value : i32) count(%n)
  return
}
// -----
func.func @scalable_fill(%dst: !reussir.ref<!reussir.array<4 x vector<[4]xi32>>>, %value: vector<[4]xi32>, %n: index) {
  // expected-error @+1 {{array elements must have a fixed size}}
  reussir.array.fill_pattern(%dst : !reussir.ref<!reussir.array<4 x vector<[4]xi32>>>) init(%value : vector<[4]xi32>) count(%n)
  return
}
// -----
func.func @unsized_fill(%dst: !reussir.ref<!reussir.array<4 x !reussir.array<? x i32>>>, %value: !reussir.array<? x i32>, %n: index) {
  // expected-error @+1 {{array elements must have a fixed size}}
  reussir.array.fill_pattern(%dst : !reussir.ref<!reussir.array<4 x !reussir.array<? x i32>>>) init(%value : !reussir.array<? x i32>) count(%n)
  return
}

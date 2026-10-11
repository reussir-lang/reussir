// RUN: %reussir-opt %s -verify-diagnostics -split-input-file

func.func @plain(%expected: i64, %desired: i64, %cell: !reussir.rc<!reussir.cell<i64>>) {
  // expected-error @+1 {{compare-and-exchange requires an atomic cell}}
  %old, %ok = reussir.cell.cmpxchg(%expected : i64, %desired : i64, %cell : !reussir.rc<!reussir.cell<i64>>) : i64, i1
  return
}

// -----

func.func @exclusive(%expected: i64, %desired: i64, %cell: !reussir.rc<!reussir.cell<i64 exclusive>>) {
  // expected-error @+1 {{compare-and-exchange requires an atomic cell}}
  %old, %ok = reussir.cell.cmpxchg(%expected : i64, %desired : i64, %cell : !reussir.rc<!reussir.cell<i64 exclusive>>) : i64, i1
  return
}

// -----

func.func @locked(%expected: i64, %desired: i64, %cell: !reussir.rc<!reussir.cell<i64 mutex> atomic>) {
  // expected-error @+1 {{compare-and-exchange requires an atomic cell}}
  %old, %ok = reussir.cell.cmpxchg(%expected : i64, %desired : i64, %cell : !reussir.rc<!reussir.cell<i64 mutex> atomic>) : i64, i1
  return
}

// -----

func.func @mismatch(%expected: i32, %desired: i32, %cell: !reussir.rc<!reussir.cell<i64 atomic> atomic>) {
  // expected-error @+1 {{expected, desired, and observed types must match the cell element type 'i64'}}
  %old, %ok = reussir.cell.cmpxchg(%expected : i32, %desired : i32, %cell : !reussir.rc<!reussir.cell<i64 atomic> atomic>) : i32, i1
  return
}

// -----

func.func @ordering(%expected: i64, %desired: i64, %cell: !reussir.rc<!reussir.cell<i64 atomic> atomic>) {
  // expected-error @+1 {{atomic ordering 'unordered' is invalid for an atomic read-modify-write}}
  %old, %ok = reussir.cell.cmpxchg(%expected : i64, %desired : i64, %cell : !reussir.rc<!reussir.cell<i64 atomic> atomic>) ordering(unordered) : i64, i1
  return
}

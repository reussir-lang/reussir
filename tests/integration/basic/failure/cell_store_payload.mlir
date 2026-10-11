// RUN: %reussir-opt %s -verify-diagnostics -split-input-file

func.func @not_cell(%value: i64, %cell: !reussir.rc<i64>) {
  // expected-error @+1 {{expected an RC pointer to a cell}}
  reussir.cell.store_payload(%value : i64, %cell : !reussir.rc<i64>)
  return
}

// -----

func.func @regional(%value: i64, %cell: !reussir.rc<!reussir.cell<i64> flex>) {
  // expected-error @+1 {{cell RC pointer must have shared capability}}
  reussir.cell.store_payload(%value : i64, %cell : !reussir.rc<!reussir.cell<i64> flex>)
  return
}

// -----

func.func @mismatch(%value: i32, %cell: !reussir.rc<!reussir.cell<i64>>) {
  // expected-error @+1 {{value type must match Cell payload type}}
  reussir.cell.store_payload(%value : i32, %cell : !reussir.rc<!reussir.cell<i64>>)
  return
}

// -----

func.func @atomic(%value: i64, %cell: !reussir.rc<!reussir.cell<i64 atomic> atomic>) {
  // expected-error @+1 {{requires a plain or exclusive Cell}}
  reussir.cell.store_payload(%value : i64, %cell : !reussir.rc<!reussir.cell<i64 atomic> atomic>)
  return
}

// -----

func.func @mutex(%value: i64, %cell: !reussir.rc<!reussir.cell<i64 mutex> atomic>) {
  // expected-error @+1 {{requires a plain or exclusive Cell}}
  reussir.cell.store_payload(%value : i64, %cell : !reussir.rc<!reussir.cell<i64 mutex> atomic>)
  return
}

// -----

func.func @flatlock(%value: i64, %cell: !reussir.rc<!reussir.cell<i64 flatlock> atomic>) {
  // expected-error @+1 {{requires a plain or exclusive Cell}}
  reussir.cell.store_payload(%value : i64, %cell : !reussir.rc<!reussir.cell<i64 flatlock> atomic>)
  return
}

// -----

func.func @rwlock(%value: i64, %cell: !reussir.rc<!reussir.cell<i64 rwlock> atomic>) {
  // expected-error @+1 {{requires a plain or exclusive Cell}}
  reussir.cell.store_payload(%value : i64, %cell : !reussir.rc<!reussir.cell<i64 rwlock> atomic>)
  return
}

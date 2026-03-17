// RUN: %reussir-opt %s --reussir-unique-carrying-recursion-analysis | %FileCheck %s

!arr = !reussir.array<1 x !reussir.rc<i64>>

module {
  // CHECK-LABEL: func.func @extract_carry(
  // CHECK: attributes {reussir.carrying_uniqueness}
  func.func @extract_carry(%rc : !reussir.rc<i64>) -> !reussir.rc<i64> {
    %arr = reussir.array.create(%rc : !reussir.rc<i64>) : !arr
    %elt = reussir.array.extract(%arr : !arr)[0] : !reussir.rc<i64>
    return %elt : !reussir.rc<i64>
  }

  // CHECK-LABEL: func.func @insert_extract_carry(
  // CHECK: attributes {reussir.carrying_uniqueness}
  func.func @insert_extract_carry(%rc : !reussir.rc<i64>) -> !reussir.rc<i64> {
    %zero = arith.constant 0 : i64
    %seed = reussir.rc.create value(%zero : i64) : !reussir.rc<i64>
    %arr0 = reussir.array.create(%seed : !reussir.rc<i64>) : !arr
    %arr1 = reussir.array.insert(%arr0 : !arr)[0](%rc : !reussir.rc<i64>) : !arr
    %elt = reussir.array.extract(%arr1 : !arr)[0] : !reussir.rc<i64>
    return %elt : !reussir.rc<i64>
  }

  // CHECK-LABEL: func.func @project_load_carry(
  // CHECK: attributes {reussir.carrying_uniqueness}
  func.func @project_load_carry(%arr_rc : !reussir.rc<!arr>) -> !reussir.rc<i64> {
    %c0 = arith.constant 0 : index
    %arr_ref = reussir.rc.borrow (%arr_rc : !reussir.rc<!arr>) : !reussir.ref<!arr shared>
    %elt_ref = reussir.array.project (%arr_ref : !reussir.ref<!arr shared>, %c0 : index) : !reussir.ref<!reussir.rc<i64> shared>
    %elt = reussir.ref.load (%elt_ref : !reussir.ref<!reussir.rc<i64> shared>) : !reussir.rc<i64>
    return %elt : !reussir.rc<i64>
  }
}

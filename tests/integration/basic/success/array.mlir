// RUN: %reussir-opt %s | %reussir-opt

!arr_i64 = !reussir.array<2 x i64>
!arr_rc = !reussir.array<1 x !reussir.rc<i64>>

module {
  func.func private @build_array(%lhs : i64, %rhs : i64) -> !arr_i64 {
    %arr = reussir.array.create(%lhs, %rhs : i64, i64) : !arr_i64
    return %arr : !arr_i64
  }

  func.func private @extract_array(%arr : !arr_i64) -> i64 {
    %elt = reussir.array.extract(%arr : !arr_i64)[1] : i64
    return %elt : i64
  }

  func.func private @project_array(%arr : !arr_i64, %index : index) -> i64 {
    %arr_ref = reussir.ref.spilled (%arr : !arr_i64) : !reussir.ref<!arr_i64>
    %elt_ref = reussir.array.project (%arr_ref : !reussir.ref<!arr_i64>, %index : index) : !reussir.ref<i64>
    %elt = reussir.ref.load (%elt_ref : !reussir.ref<i64>) : i64
    return %elt : i64
  }

  func.func private @project_store_array(%rc : !reussir.rc<!arr_i64 flex>, %rhs : i64, %index : index) {
    %arr_ref = reussir.rc.borrow (%rc : !reussir.rc<!arr_i64 flex>) : !reussir.ref<!arr_i64 flex>
    %elt_ref = reussir.array.project (%arr_ref : !reussir.ref<!arr_i64 flex>, %index : index) : !reussir.ref<i64 field>
    reussir.ref.store (%elt_ref : !reussir.ref<i64 field>) (%rhs : i64)
    return
  }

  func.func private @insert_array(%arr : !arr_i64, %rhs : i64) -> !arr_i64 {
    %updated = reussir.array.insert(%arr : !arr_i64)[1](%rhs : i64) : !arr_i64
    return %updated : !arr_i64
  }

  func.func private @managed_array(%rc : !reussir.rc<i64>) -> !reussir.rc<!arr_rc> {
    %payload = reussir.array.create(%rc : !reussir.rc<i64>) : !arr_rc
    %array = reussir.rc.create value(%payload : !arr_rc) : !reussir.rc<!arr_rc>
    return %array : !reussir.rc<!arr_rc>
  }
}

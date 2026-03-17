// RUN: %reussir-opt %s --reussir-unique-carrying-recursion-analysis | %FileCheck %s --check-prefix=UNIQUE
// RUN: %reussir-opt %s --reussir-unique-carrying-recursion-analysis --reussir-lowering-scf-ops --reussir-lowering-basic-ops | %FileCheck %s --check-prefix=LLVM

!arr = !reussir.array<32 x f32>

module {
  func.func @inc_1(%arr : !reussir.rc<!arr>) -> !reussir.rc<!arr> {
    %c0 = arith.constant 0 : index
    %res = func.call @inc_1_impl(%arr, %c0) : (!reussir.rc<!arr>, index) -> !reussir.rc<!arr>
    return %res : !reussir.rc<!arr>
  }

  func.func @inc_1_impl(%arr : !reussir.rc<!arr>, %i : index) -> !reussir.rc<!arr> {
    %c32 = arith.constant 32 : index
    %c1 = arith.constant 1 : index
    %done = arith.cmpi eq, %i, %c32 : index
    %res = scf.if %done -> (!reussir.rc<!arr>) {
      scf.yield %arr : !reussir.rc<!arr>
    } else {
      %i1 = arith.addi %i, %c1 : index
      %rec = func.call @inc_1_impl(%arr, %i1) : (!reussir.rc<!arr>, index) -> !reussir.rc<!arr>
      scf.yield %rec : !reussir.rc<!arr>
    }
    return %res : !reussir.rc<!arr>
  }

  func.func @inc_1_impl_flex(%arr : !reussir.rc<!arr flex>, %i : index) -> !reussir.rc<!arr flex> {
    %c32 = arith.constant 32 : index
    %c1f = arith.constant 1.0 : f32
    %c1 = arith.constant 1 : index
    %done = arith.cmpi eq, %i, %c32 : index
    %res = scf.if %done -> (!reussir.rc<!arr flex>) {
      scf.yield %arr : !reussir.rc<!arr flex>
    } else {
      %ref = reussir.rc.borrow (%arr : !reussir.rc<!arr flex>) : !reussir.ref<!arr flex>
      %elt_ref = reussir.array.project (%ref : !reussir.ref<!arr flex>, %i : index) : !reussir.ref<f32 field>
      %elt = reussir.ref.load (%elt_ref : !reussir.ref<f32 field>) : f32
      %inc = arith.addf %elt, %c1f : f32
      reussir.ref.store (%elt_ref : !reussir.ref<f32 field>) (%inc : f32)
      %i1 = arith.addi %i, %c1 : index
      %rec = func.call @inc_1_impl_flex(%arr, %i1) : (!reussir.rc<!arr flex>, index) -> !reussir.rc<!arr flex>
      scf.yield %rec : !reussir.rc<!arr flex>
    }
    return %res : !reussir.rc<!arr flex>
  }
}

// UNIQUE-LABEL: func.func @inc_1_impl(
// UNIQUE: func.call @inc_1_impl.unique(%arg0, %{{.+}}) : (!reussir.rc<!reussir.array<32 x f32>>, index) -> !reussir.rc<!reussir.array<32 x f32>>
// UNIQUE-LABEL: func.func private @inc_1_impl.unique(
// UNIQUE: reussir.rc.assume_unique(%arg0 : !reussir.rc<!reussir.array<32 x f32>>)
// UNIQUE: func.call @inc_1_impl.unique(%arg0, %{{.+}}) : (!reussir.rc<!reussir.array<32 x f32>>, index) -> !reussir.rc<!reussir.array<32 x f32>>

// LLVM-LABEL: llvm.func @inc_1_impl(
// LLVM: llvm.call @inc_1_impl.unique

// LLVM-LABEL: llvm.func @inc_1_impl_flex(
// LLVM: %[[DATA:.+]] = llvm.getelementptr %arg0[0, 3] : (!llvm.ptr) -> !llvm.ptr, !llvm.struct<(ptr, ptr, ptr, array<32 x f32>)>
// LLVM: %[[ELT:.+]] = llvm.getelementptr %[[DATA]][0, %arg1] : (!llvm.ptr, i64) -> !llvm.ptr, !llvm.array<32 x f32>
// LLVM: %[[VAL:.+]] = llvm.load %[[ELT]] : !llvm.ptr -> f32
// LLVM: %[[INC:.+]] = llvm.fadd %[[VAL]], %{{.+}} : f32
// LLVM: llvm.store %[[INC]], %[[ELT]] : f32, !llvm.ptr
// LLVM-LABEL: llvm.func internal @inc_1_impl.unique(
// LLVM: llvm.intr.assume

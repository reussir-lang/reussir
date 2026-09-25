// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,canonicalize,cse,reussir-convert-to-std,canonicalize,expand-strided-metadata,lower-affine,canonicalize,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe
// RUN: %not --crash %t.exe negative 2>&1 | %FileCheck %s --check-prefix=PANIC
// RUN: %not --crash %t.exe past end 2>&1 | %FileCheck %s --check-prefix=PANIC
// RUN: %not --crash %t.exe empty array index 2>&1 | %FileCheck %s --check-prefix=PANIC
// PANIC: Panic: array index out of bounds

!matrix = memref<2x3xi32, strided<[6, 2], offset: 6>>
!row = memref<3xi32, strided<[2], offset: ?>>
module {
  // The unused projection still has to check the dynamic index.
  func.func @probe(%view: !matrix, %i: index) {
    %row = reussir.array.project(%view : !matrix)[%i : index] : !row
    return
  }

  func.func @main(%argc: i32, %argv: !llvm.ptr) -> i32 {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %c2 = arith.constant 2 : index
    %c4 = arith.constant 4 : index
    %neg = arith.constant -1 : index
    %two = arith.constant 2 : i32
    %three = arith.constant 3 : i32
    %four = arith.constant 4 : i32
    %forty_two = arith.constant 42 : i32
    %storage = memref.alloca() : memref<4x6xi32>
    memref.store %forty_two, %storage[%c2, %c4] : memref<4x6xi32>
    %matrix = memref.subview %storage[1, 0][2, 3][1, 2]
      : memref<4x6xi32> to !matrix

    %empty_case = arith.cmpi eq, %argc, %four : i32
    scf.if %empty_case {
      %empty = memref.alloca() : memref<0xi32>
      %unused = reussir.array.project(%empty : memref<0xi32>)[%c0 : index] : !reussir.ref<i32>
    }
    %negative_case = arith.cmpi eq, %argc, %two : i32
    %past_case = arith.cmpi eq, %argc, %three : i32
    %nonnegative = arith.select %past_case, %c2, %c1 : index
    %index = arith.select %negative_case, %neg, %nonnegative : index
    func.call @probe(%matrix, %index) : (!matrix, index) -> ()

    // Check two successive projections of a nonzero-offset, strided view.
    %row = reussir.array.project(%matrix : !matrix)[%c1 : index] : !row
    %slot = reussir.array.project(%row : !row)[%c2 : index] : !reussir.ref<i32>
    %value = reussir.ref.load(%slot : !reussir.ref<i32>) : i32
    %ok = arith.cmpi eq, %value, %forty_two : i32
    cf.assert %ok, "projection lost its offset or stride"
    %zero = arith.constant 0 : i32
    return %zero : i32
  }
}

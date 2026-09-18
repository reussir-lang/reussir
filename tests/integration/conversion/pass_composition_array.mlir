// RUN: %reussir-opt %s --loop-invariant-code-motion | %FileCheck %s
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,loop-invariant-code-motion,reussir-convert-to-std,one-shot-bufferize{allow-unknown-ops},convert-bufferization-to-memref,convert-scf-to-cf,convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir | %llc -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe

!array = !reussir.array<1 x i32>
module {
  // A memref descriptor can move; a tensor value must observe each iteration's
  // store. Hoisting the tensor read instead adds the initial 9 three times.
  // CHECK-LABEL: func.func @sum
  // CHECK: reussir.array.view{{.*}}memref<1xi32>
  // CHECK: scf.for
  // CHECK: memref.store
  // CHECK: reussir.array.view{{.*}}tensor<1xi32>
  // CHECK: tensor.extract
  func.func @sum(%ref: !reussir.ref<!array>) -> i32 {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %c3 = arith.constant 3 : index
    %zero = arith.constant 0 : i32
    %one = arith.constant 1 : i32
    %total = scf.for %i = %c0 to %c3 step %c1 iter_args(%sum = %zero) -> i32 {
      %mem = reussir.array.view(%ref : !reussir.ref<!array>) : memref<1xi32>
      memref.store %one, %mem[%c0] : memref<1xi32>
      %tensor = reussir.array.view(%ref : !reussir.ref<!array>) : tensor<1xi32>
      %x = tensor.extract %tensor[%c0] : tensor<1xi32>
      %next = arith.addi %sum, %x : i32
      scf.yield %next : i32
    }
    return %total : i32
  }
  func.func @main() -> i32 {
    %size = arith.constant 32 : i64
    %slot = llvm.alloca %size x i8 {alignment = 8 : i64} : (i64) -> !llvm.ptr
    %ref = builtin.unrealized_conversion_cast %slot : !llvm.ptr to !reussir.ref<!array>
    %mem = reussir.array.view(%ref : !reussir.ref<!array>) : memref<1xi32>
    %c0 = arith.constant 0 : index
    %nine = arith.constant 9 : i32
    memref.store %nine, %mem[%c0] : memref<1xi32>
    %r = call @sum(%ref) : (!reussir.ref<!array>) -> i32
    %three = arith.constant 3 : i32
    %ok = arith.cmpi eq, %r, %three : i32
    cf.assert %ok, "LICM hoisted a tensor payload read across mutation"
    %zero = arith.constant 0 : i32
    return %zero : i32
  }
}

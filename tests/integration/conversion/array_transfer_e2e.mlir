// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-convert-to-std{expand-arrays=false},func.func(reussir-token-reuse),reussir-convert-to-std,canonicalize,cse,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/array_transfer_e2e.c -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %t.exe

!H = !reussir.array<? x f32>
!R = !reussir.rc<!H>
!V = memref<?xf32, strided<[?], offset: ?>>
!D = !reussir.rc<!reussir.array<? x f32, #reussir.target<devices = [0]>>>
func.func private @remember(!reussir.ref<!H>)
func.func private @check_storage(!reussir.ref<!H>, i32)

// Upload completes the host borrow before the original storage is released.
// Download initializes a bodyless allocation with the same proven extent.
func.func @roundtrip(%n: index, %shared: i1) {
  %zero = arith.constant 0 : index
  %one = arith.constant 1 : index
  %a = reussir.array.create extents(%n) : !R body {
  ^bb0(%i: index):
    %integer = arith.index_cast %i : index to i32
    %value = arith.sitofp %integer : i32 to f32
    reussir.scf.yield %value : f32
  }
  scf.if %shared {
    reussir.rc.inc(%a : !R)
  }
  %src = reussir.rc.borrow(%a : !R) : !reussir.ref<!H>
  func.call @remember(%src) : (!reussir.ref<!H>) -> ()
  %view = reussir.array.view(%src : !reussir.ref<!H>) : !V
  %size = memref.dim %view, %zero : !V
  %device = reussir.array.to_device %view : !V -> !D
  reussir.rc.dec(%a : !R)
  %b = reussir.array.create extents(%size) : !R
  %dst = reussir.rc.borrow(%b : !R) : !reussir.ref<!H>
  %aliased = arith.extui %shared : i1 to i32
  func.call @check_storage(%dst, %aliased) : (!reussir.ref<!H>, i32) -> ()
  %output = reussir.array.view(%dst : !reussir.ref<!H>) : !V
  reussir.array.to_host %device into %output : !D, !V
  scf.for %i = %zero to %size step %one {
    %actual = memref.load %output[%i] : !V
    %integer = arith.index_cast %i : index to i32
    %expected = arith.sitofp %integer : i32 to f32
    %ok = arith.cmpf oeq, %actual, %expected : f32
    cf.assert %ok, "roundtrip changed host values"
  }
  scf.if %shared {
    scf.for %i = %zero to %size step %one {
      %actual = memref.load %view[%i] : !V
      %integer = arith.index_cast %i : index to i32
      %expected = arith.sitofp %integer : i32 to f32
      %ok = arith.cmpf oeq, %actual, %expected : f32
      cf.assert %ok, "shared source was overwritten"
    }
    reussir.rc.dec(%a : !R)
  }
  reussir.rc.dec(%device : !D)
  reussir.rc.dec(%b : !R)
  return
}

!S = !reussir.array<2 x 3 x f32>
!SR = !reussir.rc<!S>
!SD = !reussir.rc<!reussir.array<2 x 3 x f32, #reussir.target<devices = [0]>>>
func.func @strided_roundtrip() {
  %zero = arith.constant 0 : index
  %one = arith.constant 1 : index
  %two = arith.constant 2 : index
  %three = arith.constant 3 : index
  %a = reussir.array.create extents() : !SR body {
  ^bb0(%i: index, %j: index):
    %row = arith.muli %i, %three : index
    %index = arith.addi %row, %j : index
    %integer = arith.index_cast %index : index to i32
    %value = arith.sitofp %integer : i32 to f32
    reussir.scf.yield %value : f32
  }
  %src = reussir.rc.borrow(%a : !SR) : !reussir.ref<!S>
  %view = reussir.array.view(%src : !reussir.ref<!S>) : memref<2x3xf32>
  %reversed = memref.reinterpret_cast %view to offset: [3], sizes: [2, 3], strides: [-3, 1] : memref<2x3xf32> to memref<2x3xf32, strided<[-3, 1], offset: 3>>
  %device = reussir.array.to_device %reversed : memref<2x3xf32, strided<[-3, 1], offset: 3>> -> !SD
  reussir.rc.dec(%a : !SR)
  %b = reussir.array.create extents() : !SR
  %dst = reussir.rc.borrow(%b : !SR) : !reussir.ref<!S>
  %output = reussir.array.view(%dst : !reussir.ref<!S>) : memref<2x3xf32>
  reussir.array.to_host %device into %output : !SD, memref<2x3xf32>
  scf.for %i = %zero to %two step %one {
    scf.for %j = %zero to %three step %one {
      %actual = memref.load %output[%i, %j] : memref<2x3xf32>
      %reverse = arith.subi %one, %i : index
      %row = arith.muli %reverse, %three : index
      %index = arith.addi %row, %j : index
      %integer = arith.index_cast %index : index to i32
      %expected = arith.sitofp %integer : i32 to f32
      %ok = arith.cmpf oeq, %actual, %expected : f32
      cf.assert %ok, "strided upload changed values"
    }
  }
  reussir.rc.dec(%device : !SD)
  reussir.rc.dec(%b : !SR)
  return
}

// REQUIRES: ifrt, linux, pjrt-runtime
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-ifrt-just-in-time-transform,symbol-dce,func.func(reussir-token-instantiation),reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-convert-to-std{expand-arrays=false},func.func(reussir-token-reuse),reussir-convert-to-std,canonicalize,cse,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/pjrt_call_e2e.c -o %t.exe %pjrt_runtime %extra_sys_libs
// RUN: env LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" REUSSIR_PJRT_CACHE_CONFIG="%S/../runtime/Inputs/pjrt_cache.toml" %t.exe

#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<4xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], sharding = #s>>>
module {
  memref.global "private" constant @alpha : memref<4xf32> = dense<2.0>
  memref.global "private" constant @x : memref<4xf32> = dense<[1.0, 2.0, 3.0, 4.0]>
  memref.global "private" constant @y : memref<4xf32> = dense<[10.0, 20.0, 30.0, 40.0]>
  func.func private @check(index, f32)
  func.func @examples() attributes {ifrt.function} {
    %a = memref.get_global @alpha : memref<4xf32>
    %x = memref.get_global @x : memref<4xf32>
    %y = memref.get_global @y : memref<4xf32>
    %dst = memref.alloca() : memref<4xf32>
    %ra = reussir.array.to_device %a : memref<4xf32> -> !R
    %rx = reussir.array.to_device %x : memref<4xf32> -> !R
    %ry = reussir.array.to_device %y : memref<4xf32> -> !R
    %va = reussir.array.to_ifrt %ra : !R -> !I
    %vx = reussir.array.to_ifrt %rx : !R -> !I
    %vy = reussir.array.to_ifrt %ry : !R -> !I
    %ready = "ifrt.After"(%vx, %vy) : (!I, !I) -> !ifrt.control
    %out, %done = ifrt.Call @saxpy::@main(%va, %vx, %vy) after %ready on devices [0] : (!I, !I, !I) -> !I
    %result = reussir.array.from_ifrt %out : !I -> !R
    %again, %done2 = ifrt.Call @saxpy::@main(%va, %vx, %vy) after %done on devices [0] : (!I, !I, !I) -> !I
    %result2 = reussir.array.from_ifrt %again : !I -> !R
    %sum, %diff, %done3 = ifrt.Call @pair::@main(%vx, %vy) after %done2 on devices [0] : (!I, !I) -> (!I, !I)
    %rs = reussir.array.from_ifrt %sum : !I -> !R
    %rd = reussir.array.from_ifrt %diff : !I -> !R
    %end = ifrt.Call @empty::@main() after %done3 on devices [0] : () -> ()
    reussir.array.to_host %result into %dst : !R, memref<4xf32>
    %c0 = arith.constant 0 : index
    %c4 = arith.constant 4 : index
    %c1 = arith.constant 1 : index
    scf.for %i = %c0 to %c4 step %c1 {
      %v = memref.load %dst[%i] : memref<4xf32>
      func.call @check(%i, %v) : (index, f32) -> ()
    }
    // Reusing the same inputs tests the synchronous, non-donating contract.
    reussir.array.to_host %result2 into %dst : !R, memref<4xf32>
    scf.for %i = %c0 to %c4 step %c1 {
      %v = memref.load %dst[%i] : memref<4xf32>
      func.call @check(%i, %v) : (index, f32) -> ()
    }
    reussir.array.to_host %rs into %dst : !R, memref<4xf32>
    scf.for %i = %c0 to %c4 step %c1 {
      %v = memref.load %dst[%i] : memref<4xf32>
      %id = arith.addi %i, %c4 : index
      func.call @check(%id, %v) : (index, f32) -> ()
    }
    reussir.array.to_host %rd into %dst : !R, memref<4xf32>
    %c8 = arith.constant 8 : index
    scf.for %i = %c0 to %c4 step %c1 {
      %v = memref.load %dst[%i] : memref<4xf32>
      %id = arith.addi %i, %c8 : index
      func.call @check(%id, %v) : (index, f32) -> ()
    }
    reussir.rc.dec(%ra : !R)
    reussir.rc.dec(%rx : !R)
    reussir.rc.dec(%ry : !R)
    reussir.rc.dec(%result : !R)
    reussir.rc.dec(%result2 : !R)
    reussir.rc.dec(%rs : !R)
    reussir.rc.dec(%rd : !R)
    return
  }
  module @saxpy attributes {sym_visibility = "private"} {
    func.func @main(%a: tensor<4xf32>, %x: tensor<4xf32>, %y: tensor<4xf32>) -> tensor<4xf32> {
      %ax = stablehlo.multiply %a, %x : tensor<4xf32>
      %out = stablehlo.add %ax, %y : tensor<4xf32>
      return %out : tensor<4xf32>
    }
  }
  module @pair attributes {sym_visibility = "private"} {
    func.func @main(%x: tensor<4xf32>, %y: tensor<4xf32>) -> (tensor<4xf32>, tensor<4xf32>) {
      %sum = stablehlo.add %x, %y : tensor<4xf32>
      %diff = stablehlo.subtract %x, %y : tensor<4xf32>
      return %sum, %diff : tensor<4xf32>, tensor<4xf32>
    }
  }
  module @empty attributes {sym_visibility = "private"} {
    func.func @main() { return }
  }
}

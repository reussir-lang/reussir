// REQUIRES: ifrt, linux, pjrt-runtime
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-ifrt-just-in-time-transform,symbol-dce,func.func(reussir-token-instantiation),reussir-pjrt-donation,reussir-rc-decrement-expansion,reussir-acquire-drop-expansion,reussir-convert-to-std{expand-arrays=false},func.func(reussir-token-reuse),reussir-convert-to-std,canonicalize,cse,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts,canonicalize)' -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/donation_e2e.c -o %t.exe %pjrt_runtime %extra_sys_libs
// RUN: env LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" REUSSIR_PJRT_CACHE_CONFIG="%S/../runtime/Inputs/pjrt_cache.toml" %t.exe
// RUN: %rrc %s -o %t.rrc.o -O aggressive --relocation-mode pic
// RUN: %cc %t.rrc.o %S/donation_e2e.c -o %t.rrc.exe %pjrt_runtime %extra_sys_libs
// RUN: env LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" REUSSIR_PJRT_CACHE_CONFIG="%S/../runtime/Inputs/pjrt_cache.toml" %t.rrc.exe

#s = #ifrt.sharding_param<1 to [0] on 1>
!I = !ifrt.array<tensor<4xf32>, #s, [0]>
!R = !reussir.rc<!reussir.array<4 x f32, #reussir.target<devices = [0], sharding = #s>>>
module {
  memref.global "private" constant @x : memref<4xf32> = dense<[1.0, 2.0, 3.0, 4.0]>
  func.func private @check(index, index, f32)
  func.func @consume(%a: !R) -> !R attributes {ifrt.function} {
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    return %r : !R
  }
  func.func @duplicate(%a: !R) -> !R attributes {ifrt.function} {
    %v = reussir.array.to_ifrt %a : !R -> !I
    %o, %done = ifrt.Call @pair::@main(%v, %v) on devices [0] : (!I, !I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    return %r : !R
  }
  func.func @dynamic_alias(%a: !R, %b: !R) -> !R attributes {ifrt.function} {
    %v = reussir.array.to_ifrt %a : !R -> !I
    %w = reussir.array.to_ifrt %b : !R -> !I
    %o, %done = ifrt.Call @pair::@main(%v, %w) on devices [0] : (!I, !I) -> !I
    %r = reussir.array.from_ifrt %o : !I -> !R
    reussir.rc.dec(%a : !R)
    reussir.rc.dec(%b : !R)
    return %r : !R
  }
  func.func @loop(%a: !R, %n: index) -> !R attributes {ifrt.function} {
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    %result = scf.for %i = %zero to %n step %one iter_args(%state = %a) -> !R {
      %v = reussir.array.to_ifrt %state : !R -> !I
      %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
      %next = reussir.array.from_ifrt %o : !I -> !R
      reussir.rc.dec(%state : !R)
      scf.yield %next : !R
    }
    return %result : !R
  }
  func.func @while_loop(%a: !R, %n: index) -> !R attributes {ifrt.function} {
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    %result:2 = scf.while (%state = %a, %i = %zero) : (!R, index) -> (!R, index) {
      %continue = arith.cmpi slt, %i, %n : index
      scf.condition(%continue) %state, %i : !R, index
    } do {
    ^bb0(%state: !R, %i: index):
      %v = reussir.array.to_ifrt %state : !R -> !I
      %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
      %next = reussir.array.from_ifrt %o : !I -> !R
      reussir.rc.dec(%state : !R)
      %j = arith.addi %i, %one : index
      scf.yield %next, %j : !R, index
    }
    return %result#0 : !R
  }
  func.func @branch(%a: !R, %choose: i1) -> !R attributes {ifrt.function} {
    %result = scf.if %choose -> !R {
      %v = reussir.array.to_ifrt %a : !R -> !I
      %o, %done = ifrt.Call @kernel::@main(%v) on devices [0] : (!I) -> !I
      %next = reussir.array.from_ifrt %o : !I -> !R
      reussir.rc.dec(%a : !R)
      scf.yield %next : !R
    } else {
      %v = reussir.array.to_ifrt %a : !R -> !I
      %o, %done = ifrt.Call @pair::@main(%v, %v) on devices [0] : (!I, !I) -> !I
      %next = reussir.array.from_ifrt %o : !I -> !R
      reussir.rc.dec(%a : !R)
      scf.yield %next : !R
    }
    return %result : !R
  }
  func.func @verify(%a: !R, %id: index) {
    %dst = memref.alloca() : memref<4xf32>
    reussir.array.to_host %a into %dst : !R, memref<4xf32>
    %zero = arith.constant 0 : index
    %one = arith.constant 1 : index
    %four = arith.constant 4 : index
    scf.for %i = %zero to %four step %one {
      %v = memref.load %dst[%i] : memref<4xf32>
      func.call @check(%id, %i, %v) : (index, index, f32) -> ()
    }
    reussir.rc.dec(%a : !R)
    return
  }
  func.func @examples() attributes {ifrt.function} {
    %host = memref.get_global @x : memref<4xf32>
    %zero = arith.constant 0 : index
    %five = arith.constant 5 : index
    %yes = arith.constant true
    %no = arith.constant false
    %a0 = reussir.array.to_device %host : memref<4xf32> -> !R
    %r0 = func.call @consume(%a0) : (!R) -> !R
    %id0 = arith.constant 0 : index
    func.call @verify(%r0, %id0) : (!R, index) -> ()
    %a1 = reussir.array.to_device %host : memref<4xf32> -> !R
    reussir.rc.inc(%a1 : !R)
    %r1 = func.call @consume(%a1) : (!R) -> !R
    %id1 = arith.constant 1 : index
    func.call @verify(%r1, %id1) : (!R, index) -> ()
    %sharedid = arith.constant 10 : index
    func.call @verify(%a1, %sharedid) : (!R, index) -> ()
    %a2 = reussir.array.to_device %host : memref<4xf32> -> !R
    %r2 = func.call @duplicate(%a2) : (!R) -> !R
    %id2 = arith.constant 2 : index
    func.call @verify(%r2, %id2) : (!R, index) -> ()
    %a3 = reussir.array.to_device %host : memref<4xf32> -> !R
    reussir.rc.inc(%a3 : !R)
    %r3 = func.call @dynamic_alias(%a3, %a3) : (!R, !R) -> !R
    %id3 = arith.constant 3 : index
    func.call @verify(%r3, %id3) : (!R, index) -> ()
    %a4 = reussir.array.to_device %host : memref<4xf32> -> !R
    %r4 = func.call @loop(%a4, %zero) : (!R, index) -> !R
    %id4 = arith.constant 4 : index
    func.call @verify(%r4, %id4) : (!R, index) -> ()
    %a5 = reussir.array.to_device %host : memref<4xf32> -> !R
    %r5 = func.call @loop(%a5, %five) : (!R, index) -> !R
    %id5 = arith.constant 5 : index
    func.call @verify(%r5, %id5) : (!R, index) -> ()
    %a6 = reussir.array.to_device %host : memref<4xf32> -> !R
    %r6 = func.call @while_loop(%a6, %zero) : (!R, index) -> !R
    %id6 = arith.constant 6 : index
    func.call @verify(%r6, %id6) : (!R, index) -> ()
    %a7 = reussir.array.to_device %host : memref<4xf32> -> !R
    %r7 = func.call @while_loop(%a7, %five) : (!R, index) -> !R
    %id7 = arith.constant 7 : index
    func.call @verify(%r7, %id7) : (!R, index) -> ()
    %a8 = reussir.array.to_device %host : memref<4xf32> -> !R
    %r8 = func.call @branch(%a8, %yes) : (!R, i1) -> !R
    %id8 = arith.constant 8 : index
    func.call @verify(%r8, %id8) : (!R, index) -> ()
    %a9 = reussir.array.to_device %host : memref<4xf32> -> !R
    %r9 = func.call @branch(%a9, %no) : (!R, i1) -> !R
    %id9 = arith.constant 9 : index
    func.call @verify(%r9, %id9) : (!R, index) -> ()
    %twina = reussir.array.to_device %host : memref<4xf32> -> !R
    %twinv = reussir.array.to_ifrt %twina : !R -> !I
    %twinx, %twiny, %twindone = ifrt.Call @twins::@main(%twinv) on devices [0] : (!I) -> (!I, !I)
    %twinr = reussir.array.from_ifrt %twinx : !I -> !R
    %twinsibling = reussir.array.from_ifrt %twiny : !I -> !R
    reussir.rc.dec(%twina : !R)
    %twinresult = func.call @consume(%twinr) : (!R) -> !R
    %eleven = arith.constant 11 : index
    %twelve = arith.constant 12 : index
    func.call @verify(%twinresult, %eleven) : (!R, index) -> ()
    func.call @verify(%twinsibling, %twelve) : (!R, index) -> ()
    return
  }
  module @twins attributes {sym_visibility = "private"} {
    func.func @main(%x: tensor<4xf32>) -> (tensor<4xf32>, tensor<4xf32>) {
      return %x, %x : tensor<4xf32>, tensor<4xf32>
    }
  }
  module @kernel attributes {sym_visibility = "private"} {
    func.func @main(%x: tensor<4xf32>) -> tensor<4xf32> {
      %one = stablehlo.constant dense<1.0> : tensor<4xf32>
      %r = stablehlo.add %x, %one : tensor<4xf32>
      return %r : tensor<4xf32>
    }
  }
  module @pair attributes {sym_visibility = "private"} {
    func.func @main(%x: tensor<4xf32>, %y: tensor<4xf32>) -> tensor<4xf32> {
      %r = stablehlo.add %x, %y : tensor<4xf32>
      return %r : tensor<4xf32>
    }
  }
}

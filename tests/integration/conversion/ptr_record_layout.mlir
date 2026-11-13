// RUN: %reussir-opt %s --reussir-attach-native-target | %FileCheck %s --check-prefix=SPEC
// RUN: %reussir-opt %s --pass-pipeline='builtin.module(reussir-attach-native-target,reussir-convert-to-std,convert-scf-to-cf,reussir-lowering-basic-ops,reussir-convert-to-llvm,reconcile-unrealized-casts)' | %reussir-translate --mlir-to-llvmir | %FileCheck %s
// REQUIRES: pointer64

// The pointer spec gives `!ptr.ptr` the layout of the target's pointers, so a
// record holding one is ordered and sized as its LLVM struct is.
// SPEC: !ptr.ptr<#ptr.generic_space> = #ptr.spec<size = 64, abi = 64, preferred = 64, index = 64>

!P = !ptr.ptr<#ptr.generic_space>
!R = !reussir.record<compound "Holder" [value] { i8, !P, i8 }>

// The verifier checks token layouts on parsing, before the native target is
// attached, so the input carries a pointer spec of its own.
module attributes {dlti.dl_spec = #dlti.dl_spec<!P = #ptr.spec<size = 64, abi = 64, preferred = 64>>} {
  // CHECK: %Holder = type { ptr, i8, i8, [6 x i8] }

  // CHECK-LABEL: define ptr @wrap(ptr %0)
  // CHECK: call ptr @__reussir_allocate_small(i64 24)
  func.func @wrap(%p: !P) -> !reussir.rc<!R> {
    %a = arith.constant 1 : i8
    %r = reussir.record.compound(%a, %p, %a : i8, !P, i8) : !R
    %t = reussir.token.alloc : !reussir.token<align: 8, size: 24>
    %rc = reussir.rc.create value(%r : !R) token(%t : !reussir.token<align: 8, size: 24>) : !reussir.rc<!R>
    return %rc : !reussir.rc<!R>
  }

  // CHECK-LABEL: define ptr @field(ptr %0)
  // CHECK: %[[FIELD:.*]] = getelementptr %Holder, ptr %0, i32 0, i32 0
  // CHECK: load ptr, ptr %[[FIELD]], align 8
  func.func @field(%r: !reussir.ref<!R>) -> !P {
    %f = reussir.ref.project(%r : !reussir.ref<!R>) [1] : !reussir.ref<!P>
    %p = reussir.ref.load(%f : !reussir.ref<!P>) : !P
    return %p : !P
  }
}

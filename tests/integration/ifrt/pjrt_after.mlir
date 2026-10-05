// REQUIRES: ifrt
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform | %FileCheck %s --check-prefix=JIT --implicit-check-not=ifrt.function --implicit-check-not=ifrt.After
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --reussir-convert-to-llvm --reconcile-unrealized-casts | %FileCheck %s --check-prefix=LLVM --implicit-check-not=ifrt. --implicit-check-not=reussir.pjrt

#s = #ifrt.sharding_param<2 to [0] on 2>
!I = !ifrt.array<tensor<8xf32>, #s, [1, 0]>
!R = !reussir.rc<!reussir.array<8 x f32, #reussir.target<devices = [1, 0], sharding = #s>>>
// JIT-LABEL: func.func @ready
// JIT: reussir.pjrt.after
// LLVM-LABEL: llvm.func @ready
// LLVM: llvm.getelementptr {{.*}}[0] : (!llvm.ptr) -> !llvm.ptr, !llvm.ptr
// LLVM: %[[FIRST:.*]] = llvm.load
// LLVM: llvm.call @__reussir_pjrt_array_wait_ready(%[[FIRST]])
// LLVM: llvm.getelementptr {{.*}}[1] : (!llvm.ptr) -> !llvm.ptr, !llvm.ptr
// LLVM: %[[SECOND:.*]] = llvm.load
// LLVM: llvm.call @__reussir_pjrt_array_wait_ready(%[[SECOND]])
// LLVM: llvm.return
func.func @ready(%input: !R) attributes {ifrt.function} {
  %view = reussir.array.to_ifrt %input : !R -> !I
  %ready = "ifrt.After"(%view) : (!I) -> !ifrt.control
  return
}

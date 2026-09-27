// RUN: %reussir-opt %s | %reussir-opt | %FileCheck %s --check-prefix=PARSE
// RUN: %reussir-opt %s --emit-bytecode -o %t.bc
// RUN: %reussir-opt %t.bc | %FileCheck %s --check-prefix=PARSE
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --reussir-token-reuse | %FileCheck %s --check-prefix=TOKEN --implicit-check-not=reussir.token --implicit-check-not='token('
// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --reussir-rc-decrement-expansion --reussir-acquire-drop-expansion --reussir-convert-to-std --convert-scf-to-cf --reussir-lowering-basic-ops --convert-to-llvm --reconcile-unrealized-casts | %FileCheck %s --check-prefix=LLVM

#device = #reussir.target<devices = [0]>
!array = !reussir.array<? x 4 x f32, #device>
!rc = !reussir.rc<!array>
!atomic = !reussir.rc<!array atomic>

// PARSE: !reussir.array<? x 4 x f32, #reussir.target<devices = [0]>>
// TOKEN-LABEL: func.func @create
// TOKEN: reussir.array.create extents(%arg0) :
// LLVM-LABEL: llvm.func @create
// LLVM: llvm.alloca {{.*}} x i64
// LLVM: llvm.call @__reussir_pjrt_array_allocate
// LLVM: llvm.mlir.constant(32 : i64)
// LLVM: llvm.call @__reussir_allocate
// LLVM: !llvm.struct<(i32, struct<(ptr, i64, i64)>)>
func.func @create(%n: index) -> !rc {
  %array = reussir.array.create extents(%n) : !rc
  return %array : !rc
}

// TOKEN-LABEL: func.func @release
// TOKEN: reussir.rc.dec(%arg0
// LLVM-LABEL: llvm.func @release
// LLVM: llvm.icmp "eq"
// LLVM: llvm.cond_br
// LLVM: llvm.call @__reussir_pjrt_array_deallocate
// LLVM: llvm.call @__reussir_dealloc_unsized
func.func @release(%array: !rc) {
  reussir.rc.dec(%array : !rc)
  return
}

// LLVM-LABEL: llvm.func @release_atomic
// LLVM: llvm.atomicrmw sub {{.*}} acq_rel
// LLVM: llvm.call @__reussir_pjrt_array_deallocate
// LLVM: llvm.call @__reussir_dealloc_unsized
func.func @release_atomic(%array: !atomic) {
  reussir.rc.dec(%array : !atomic)
  return
}

// LLVM-LABEL: llvm.func @drop_descriptor
// LLVM: llvm.call @__reussir_pjrt_array_deallocate
func.func @drop_descriptor(%array: !reussir.ref<!array>) {
  reussir.ref.drop(%array : !reussir.ref<!array>)
  return
}

// A creation site inside a loop uses one entry-block shape buffer.
// LLVM-LABEL: llvm.func @loop
// LLVM: llvm.alloca
// LLVM: llvm.br
// LLVM-NOT: llvm.alloca
// LLVM: llvm.call @__reussir_pjrt_array_allocate
func.func @loop(%n: index) {
  %zero = arith.constant 0 : index
  %one = arith.constant 1 : index
  scf.for %i = %zero to %n step %one {
    %a = reussir.array.create extents(%n) : !rc
    reussir.rc.dec(%a : !rc)
  }
  return
}

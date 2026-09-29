// RUN: %reussir-opt %s --reussir-attach-native-target --reussir-token-instantiation --reussir-convert-to-llvm --reconcile-unrealized-casts --canonicalize -o %t.mlir
// RUN: %reussir-translate --mlir-to-llvmir %t.mlir | %opt -S -O2 -o %t.ll
// RUN: %llc %t.ll -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/array_transfer_checked.c -o %t.exe -L%library_path -lreussir_rt %rpath_flag %extra_sys_libs
// RUN: %if wine-quirks %{ %not %} %else %{ %not --crash %} %t.exe shape 2>&1 | %FileCheck %s --check-prefix=SHAPE
// RUN: %if wine-quirks %{ %not %} %else %{ %not --crash %} %t.exe stride 2>&1 | %FileCheck %s --check-prefix=STRIDE
// RUN: %if wine-quirks %{ %not %} %else %{ %not --crash %} %t.exe overflow 2>&1 | %FileCheck %s --check-prefix=OVERFLOW
// RUN: %if wine-quirks %{ %not %} %else %{ %not --crash %} %t.exe negative 2>&1 | %FileCheck %s --check-prefix=NEGATIVE
// RUN: %if wine-quirks %{ %not %} %else %{ %not --crash %} %t.exe offset 2>&1 | %FileCheck %s --check-prefix=OFFSET
// RUN: %if wine-quirks %{ %not %} %else %{ %not --crash %} %t.exe upload_stride 2>&1 | %FileCheck %s --check-prefix=OVERFLOW
// RUN: %if wine-quirks %{ %not %} %else %{ %not --crash %} %t.exe upload_negative 2>&1 | %FileCheck %s --check-prefix=NEGATIVE
// RUN: %if wine-quirks %{ %not %} %else %{ %not --crash %} %t.exe bytes 2>&1 | %FileCheck %s --check-prefix=BYTES

// Wine reports abort as an ordinary nonzero exit; also require the specific
// guard diagnostic so loader failures or unrelated exits cannot pass.
// SHAPE: array transfer shape mismatch
// STRIDE: array.to_host requires dense row-major storage
// OVERFLOW: array transfer size or stride overflow
// NEGATIVE: array transfer extent must be nonnegative
// OFFSET: array transfer requires a complete device buffer
// BYTES: array transfer host byte size mismatch

!D = !reussir.rc<!reussir.array<? x ? x f32, #reussir.target<devices = [0]>>>
!V = memref<?x?xf32, strided<[?, ?], offset: ?>>
func.func @download(%array: !D, %dst: !V) attributes {llvm.emit_c_interface} {
  reussir.array.to_host %array into %dst : !D, !V
  return
}
func.func @upload(%src: !V) -> !D attributes {llvm.emit_c_interface} {
  %array = reussir.array.to_device %src : !V -> !D
  return %array : !D
}

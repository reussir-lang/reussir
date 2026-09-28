// REQUIRES: linux, pjrt-runtime
// RUN: %reussir-translate --mlir-to-llvmir %s | %llc -relocation-model=pic -filetype=obj -o %t.o
// RUN: %cc %t.o %S/pjrt_transfer.c "%pjrt_runtime" -o %t.exe
// RUN: env LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %t.exe
// RUN: env LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %not --crash %t.exe bad-layout 2>&1 | %FileCheck %s --check-prefix=BAD-LAYOUT
// RUN: env LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %not --crash %t.exe bad-memory 2>&1 | %FileCheck %s --check-prefix=BAD-MEMORY
// BAD-LAYOUT: PjRt host layout mixes strides and tiles
// BAD-MEMORY: no memory kind

// Exercise the compiler-facing C ABI with dynamically supplied shape/stride
// arrays, placement options, and the same host layout for size and download.
llvm.func @__reussir_pjrt_array_from_host(i64, i32, !llvm.ptr, i64, !llvm.ptr, !llvm.ptr, !llvm.ptr) -> !llvm.ptr
llvm.func @__reussir_pjrt_array_host_size(!llvm.ptr, !llvm.ptr) -> i64
llvm.func @__reussir_pjrt_array_to_host(!llvm.ptr, !llvm.ptr, i64, !llvm.ptr)
llvm.func @__reussir_pjrt_array_deallocate(!llvm.ptr)

llvm.func @transfer(%data: !llvm.ptr, %dims: !llvm.ptr, %strides: !llvm.ptr,
                    %options: !llvm.ptr, %layout: !llvm.ptr, %out: !llvm.ptr) -> i64 {
  %device = llvm.mlir.constant(0 : i64) : i64
  %type = llvm.mlir.constant(4 : i32) : i32 // PJRT_Buffer_Type_S32
  %rank = llvm.mlir.constant(2 : i64) : i64
  %buffer = llvm.call @__reussir_pjrt_array_from_host(%device, %type, %dims, %rank, %data, %strides, %options) : (i64, i32, !llvm.ptr, i64, !llvm.ptr, !llvm.ptr, !llvm.ptr) -> !llvm.ptr
  %bytes = llvm.call @__reussir_pjrt_array_host_size(%buffer, %layout) : (!llvm.ptr, !llvm.ptr) -> i64
  llvm.call @__reussir_pjrt_array_to_host(%buffer, %out, %bytes, %layout) : (!llvm.ptr, !llvm.ptr, i64, !llvm.ptr) -> ()
  llvm.call @__reussir_pjrt_array_deallocate(%buffer) : (!llvm.ptr) -> ()
  llvm.return %bytes : i64
}

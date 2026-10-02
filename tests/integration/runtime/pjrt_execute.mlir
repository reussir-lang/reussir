// REQUIRES: linux, stablehlo, pjrt-runtime
// RUN: %reussir-opt %s --emit-bytecode -o %t.bc
// RUN: %reussir-opt %S/Inputs/pjrt_execute_empty.mlir --emit-bytecode -o %t.empty.bc
// RUN: pjrt_test_blake3=$(ls -t "%pjrt_runtime_dir"/deps/libblake3-*.rlib | head -n 1) && %rustc_path --edition=2024 -C prefer-dynamic %S/Inputs/pjrt_execute.rs --extern blake3="$pjrt_test_blake3" --extern blake3="${pjrt_test_blake3%.rlib}.rmeta" -L "%pjrt_runtime_dir/deps" -L native="%pjrt_runtime_dir" -l dylib=reussir_rt -o %t.exe
// RUN: env REUSSIR_PJRT_CACHE_CONFIG="%S/Inputs/pjrt_cache.toml" LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %t.exe %t.bc %t.empty.bc
// RUN: env REUSSIR_PJRT_CACHE_CONFIG="%S/Inputs/pjrt_cache.toml" LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %not --crash %t.exe %t.bc %t.empty.bc wrong-output-count 2>&1 | %FileCheck %s --check-prefix=OUTPUTS
// RUN: env REUSSIR_PJRT_CACHE_CONFIG="%S/Inputs/pjrt_cache.toml" LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %not --crash %t.exe %t.bc %t.empty.bc wrong-input-count 2>&1 | %FileCheck %s --check-prefix=INPUTS
// OUTPUTS: executable returns 2 outputs, but invocation provides 1 slots
// INPUTS: PjRt:

// Invoke MLIR bytecode through the runtime ABI, preserving both input buffers
// across repeated calls and transferring ownership of multiple output buffers.
module {
  func.func @main(%lhs: tensor<4xf32>, %rhs: tensor<4xf32>) -> (tensor<4xf32>, tensor<4xf32>) {
    %sum = stablehlo.add %lhs, %rhs : tensor<4xf32>
    %difference = stablehlo.subtract %lhs, %rhs : tensor<4xf32>
    return %sum, %difference : tensor<4xf32>, tensor<4xf32>
  }
}

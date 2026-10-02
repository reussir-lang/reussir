// REQUIRES: linux, stablehlo, pjrt-runtime
// RUN: rm -rf %t.cache
// RUN: %reussir-opt %s --emit-bytecode -o %t.bc
// RUN: pjrt_test_blake3=$(ls -t "%pjrt_runtime_dir"/deps/libblake3-*.rlib | head -n 1) && %rustc_path --edition=2024 -C prefer-dynamic %S/Inputs/pjrt_compile.rs --extern blake3="$pjrt_test_blake3" --extern blake3="${pjrt_test_blake3%.rlib}.rmeta" -L "%pjrt_runtime_dir/deps" -L native="%pjrt_runtime_dir" -l dylib=reussir_rt -o %t.exe
// RUN: env XDG_CACHE_HOME="%t.cache" LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %t.exe %t.bc
// RUN: env XDG_CACHE_HOME="%t.cache" LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %t.exe %t.bc
// RUN: env XDG_CACHE_HOME="%t.cache" LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %not --crash %t.exe %t.bc corrupt 2>&1 | %FileCheck %s --check-prefix=CORRUPT
// RUN: env -u REUSSIR_PJRT_PLUGIN LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %not --crash %t.exe %t.bc malformed 2>&1 | %FileCheck %s --check-prefix=MALFORMED
// RUN: env XDG_CACHE_HOME="%t.cache" LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %not --crash %t.exe %t.bc bad-options 2>&1 | %FileCheck %s --check-prefix=OPTIONS
// RUN: env REUSSIR_PJRT_CACHE_CONFIG="%S/Inputs/pjrt_cache.toml" LD_LIBRARY_PATH="%pjrt_runtime_dir:$LD_LIBRARY_PATH" %t.exe %t.bc
// RUN: rm -rf %t.cache
// CORRUPT: BLAKE3 checksum does not match bytecode
// MALFORMED: requires a 64-character hexadecimal BLAKE3 checksum
// OPTIONS: PjRt:

// Test the runtime ABI directly with MLIR bytecode, including source locations.
// The supplied Cargo-built runtime provides blake3 in its sibling deps directory.
// Select a built rlib (not check-only metadata); recent rustc keeps its full
// metadata in the matching rmeta. Other build variants may share the directory.
module {
  func.func @main() -> tensor<i32> {
    %value = stablehlo.constant dense<42> : tensor<i32> loc("kernel.rr":3:5)
    return %value : tensor<i32>
  }
}

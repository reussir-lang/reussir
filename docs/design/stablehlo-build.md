# StableHLO tooling

`REUSSIR_ENABLE_OPENXLA=ON` adds StableHLO, CHLO, and VHLO dialects to
`reussir-opt`, together with upstream frontend transformation, shape refinement,
optimization, and version conversion passes. The option is disabled by default.

```sh
cmake -S . -B build -DREUSSIR_ENABLE_OPENXLA=ON
cmake --build build --target reussir-opt
python3 tests/integration/lit build/tests/integration -v --filter 'stablehlo/'
```

The `ReussirStablehlo` target uses StableHLO's embedded CMake build with the
LLVM/MLIR selected by Reussir. It does not fetch XLA or require Bazel. The source
archive is pinned to `b9029ac2228a12e176ea20e56f7d3dcb6bf8a019` and SHA-256 checked.
This revision builds against LLVM/MLIR 23.1.0; its upstream LLVM pin is a tested
baseline, not an additional version check.

For a local checkout of that revision, configure with
`-DFETCHCONTENT_SOURCE_DIR_STABLEHLO=/path/to/stablehlo`.

## Preparing tensor programs

```sh
build/bin/reussir-opt input.mlir --chlo-pre-serialization-pipeline \
  --stablehlo-target-independent-optimization
build/bin/reussir-opt input.mlir --stablehlo-legalize-to-vhlo \
  --emit-bytecode -o program.vhlo.bc
build/bin/reussir-opt program.vhlo.bc --stablehlo-deserialize
```

These transformations keep tensor programs available for further frontend
passes before backend compilation. They do not invoke an XLA device compiler.
The VHLO example writes MLIR bytecode for inspection and round trips; it is
not an IFRT program envelope.

MHLO dialect/conversion registration and StableHLO-to-Linalg pass registration
are deliberately omitted. Upstream StableHLO CMake targets may still include
Linalg implementation dependencies; those do not select a lowering route.
Optional libraries are kept out of MLIR's global link collections so they do
not enter Reussir's core compiler merely by enabling this tool integration.

See upstream [StableHLO](https://github.com/openxla/stablehlo/tree/b9029ac2228a12e176ea20e56f7d3dcb6bf8a019).

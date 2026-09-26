# IFRT tooling

`REUSSIR_ENABLE_OPENXLA=ON` enables StableHLO and IFRT together. The IFRT
build target adds IFRT/VIFRT dialects, Shardy's SDY dialect, selected preparation
passes, and program serialization. There is no separate IFRT feature option.

```sh
cmake -S . -B build -DREUSSIR_ENABLE_OPENXLA=ON
cmake --build build --target reussir-opt reussir-ifrt-translate
python3 tests/integration/lit build/tests/integration -v --filter 'ifrt/'
```

IFRT has no upstream CMake build. `cmake/ifrt` uses Bazel 8.7.0 (provided by
`nix develop`) to build upstream IFRT libraries, with Reussir's LLVM/MLIR and
StableHLO supplied by CMake. The adapter currently requires native Linux,
Clang, and the installed LLVM shared library for a host TableGen helper.
Linux x86-64 and AArch64 CI both enable StableHLO and IFRT.

Set `REUSSIR_BAZEL_EXECUTABLE` to select Bazel or Bazelisk and
`REUSSIR_IFRT_JOBS` to limit parallelism (default 8). The first build downloads
and compiles IFRT's dependencies; later invocations use
`build/cmake/ifrt/cache`.

The XLA archive is pinned to `fc840b37e26bd36c99f32e295324a1700dc12ecd` and
SHA-256 checked. For a local checkout of that revision, configure with
`-DFETCHCONTENT_SOURCE_DIR_XLA=/path/to/xla`. See also the
[StableHLO build notes](stablehlo-build.md) for its pin and source override.

## Preparing and serializing a program

`ifrt-to-outlined-atom-programs-pipeline` verifies donation and sharding,
outlines atom programs, normalizes copies/reshards, and adds control
dependencies. A tensor function called by `ifrt.Call @kernel` becomes a nested
`module @kernel` with `func.func @main`; the call becomes
`ifrt.Call @kernel::@main(...) on devices [...]`.

```sh
build/bin/reussir-opt tests/integration/ifrt/program.mlir \
  --ifrt-to-outlined-atom-programs-pipeline -o /tmp/model.mlir
build/bin/reussir-ifrt-translate /tmp/model.mlir --serialize \
  --strip_debuginfo -o /tmp/model.ifrt.bc
build/bin/reussir-ifrt-translate /tmp/model.ifrt.bc --deserialize
```

The translator uses upstream `Serialize(IfrtIRProgram, options)` and
`Deserialize<IfrtIRProgram>`. The file is the standard `Serialized` protobuf
envelope containing VIFRT orchestration bytecode and versioned StableHLO/VHLO
atom programs. `--deserialize` restores program IR; it neither compiles the
kernels nor loads a device executable.

`--ifrt_version`, `--atom_program_version`, and `--atom_program_sdy_version`
default to `current`. Upstream IFRT, StableHLO, and Shardy validate their
supported version ranges. Bare MLIR bytecode is not this program envelope.

IFRT array types carry shape, element type, sharding, logical devices,
optional memory kind, and optional layout. These describe the program;
registering the dialect does not create a runtime array. Runtime device
binding, `CompileAndLoad`, Reussir array interoperability, and CPU/GPU execution
remain subsequent work. No Linalg lowering is used in this artifact path.

The individually registered passes cover donation/sharding verification,
control dependencies, duplicate callees, copy/reshard preparation, outlining,
attribute removal, and IFRT/VIFRT version conversions. The atom-compilation
pass requires a runtime compiler and is not registered by this integration.

See the upstream
[serializer](https://github.com/openxla/xla/blob/fc840b37e26bd36c99f32e295324a1700dc12ecd/xla/python/ifrt/ir/ifrt_ir_program_serdes.cc)
and [program schema](https://github.com/openxla/xla/blob/fc840b37e26bd36c99f32e295324a1700dc12ecd/xla/python/ifrt/ir/ifrt_ir_program.proto).

## Build adapter

`ReussirIfrt` contains upstream IFRT support objects. These require XLA support
libraries (including the HLO layout parser and MHLO), but do not provide a
CPU/GPU execution backend. MHLO remains an implementation dependency and is
not registered as a public dialect or pass family in `reussir-opt`.

`build.py` stages sources in the build directory without modifying source
checkouts. Its overlays are limited to build integration and compatibility:

- LLVM/MLIR and StableHLO Bazel repositories expose CMake's headers and TableGen.
  The host TableGen helper and final tools link the installed LLVM/MLIR.
- VIFRT-to-IFRT declares its Func dialect dependency for VIFRT-only input.
- Where MLIR lacks `SymbolName`/`SymbolVisibility`, IFRT and pinned Shardy use
  the equivalent `Symbol` interface and visibility helper.
- The translation driver uses upstream serialization, with its entry point
  renamed and SDY registered to parse outlined modules containing meshes.
  Only the serializer registration archive needs CMake's `WHOLE_ARCHIVE`.

The MLIR Bazel TableGen rule is pinned to LLVM revision
`e56c2cefc3e7978de4a2d799fa3abec77d75af21` and SHA-256 checked. XLA support code
uses C++20 with the same compiler and standard-library ABI as Reussir because
this XLA revision has an invalid `std::to_underlying` alias in its C++23 path.
Reussir continues to use C++23. Keep pins and overlays together when updating.

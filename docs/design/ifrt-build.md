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
registering the dialect does not create a runtime array, and program
serialization does not execute kernels. No Linalg lowering is used in this
artifact path.

The RC/IFRT bridge operations have LLVM lowering for static and dynamic shapes.
`reussir.array.to_ifrt` borrows the device-handle descriptor from an RC array
without changing its reference count, copying data, or waiting for the device.
The array must describe a complete buffer: a nonzero offset fails at runtime.
`reussir.array.from_ifrt` adopts an owned call result into a new RC descriptor,
using an allocation token for the host metadata and preserving shard order.
It does not allocate device storage. The existing bridge verifiers enforce
matching metadata, forbid donation of borrowed inputs, and require each adopted
result to have exactly one use.

Borrowing preserves the descriptor's dynamic extents without querying the
backend. Adopting a dynamically shaped result queries each device buffer's
unpadded dimensions, checks equal local shard shapes, and reconstructs global
extents using concrete, even sharding. Static dimensions must match the array
type; checked multiplication and index-width conversion reject extent overflow.
Only dynamic extents are stored in the RC descriptor. Dimension queries may
wait for the backend to compute the shape, but do not copy data or allocate
device storage. Multi-device results require concrete sharding, and uneven
shards are not supported.

`reussir.ifrt.bytecode` lowers to an internal, immutable LLVM global containing
`{[64 x i8], [N x i8]}`: the hexadecimal BLAKE3 checksum followed by the exact
MLIR bytecode payload. Neither field includes a trailing NUL. The symbol name
is preserved so prepared `reussir.pjrt.jit_call` operations can still reference
the payload during partial lowering. Lowering an existing bytecode literal does
not require an OpenXLA-enabled build; preparing IFRT kernels does.

Prepared `reussir.pjrt.jit_call` operations lower to cached compilation,
executable-device validation, and synchronous execution through the PJRT runtime.
Execution borrows the input buffers without donation and waits for every device
before returning owned output handles. Each output must be adopted exactly once
with `reussir.array.from_ifrt`. Arrays must use all call devices in the same
order; donation and input/output aliases are rejected. The runtime validates
executable placement before accessing array descriptors.

`ifrt.After` becomes `reussir.pjrt.after`, which waits for each input shard to
materialize. Completion handles lower to already-ready values because the
runtime calls finish synchronously; the side-effecting calls preserve execution
order. This does not provide asynchronous IFRT execution or automatic donation.

For this path, explicitly run `--reussir-ifrt-just-in-time-transform` before
`--reussir-convert-to-llvm`, along with the host program's normal array, RC, and
control-flow lowerings. The native compiler pipeline does not yet schedule IFRT
preparation automatically. The `pjrt_call_e2e.mlir` and
`pjrt_sharded_e2e.mlir` integration tests show complete lowering/linking commands.
Execution requires a PJRT-enabled runtime and `REUSSIR_PJRT_PLUGIN` pointing to
a compatible backend plugin; registering the compiler dialects alone does not
provide a device backend.

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

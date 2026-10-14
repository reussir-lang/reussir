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

Borrowing preserves the descriptor's dynamic global extents without querying the
backend. When adopting a dynamically shaped result, `array.from_ifrt` accepts
`extents(%d0, %d1, ...)` for all dynamic global dimensions, in dimension order.
Explicit extents are required when a dynamic dimension is partitioned: physical
buffers of extent three on two devices cannot distinguish global extent five
from six. The caller supplies that logical shape; the compiler checks that all
buffer shapes are compatible with it and with the static dimensions, using
ceiling division by the shard counts. Unpartitioned dynamic dimensions can
still be inferred from buffer queries when explicit extents are omitted.
Negative extents, incompatible physical shapes, and index-width overflow fail
at runtime. Dimension queries may wait for the backend, but do not copy data or
allocate device storage. Only dynamic global extents are stored in the RC box.

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

The native compiler registers IFRT/StableHLO and automatically prepares device
kernels before host inlining and optimization, including at `-O none`. Compile
an outlined MLIR host program with `rrc input.mlir --emit llvm-ir -o output.ll`
or emit a native object for linking. Compilation embeds the kernel bytecode;
it does not require a PJRT runtime or load a backend plugin. The tensor kernels
remain MLIR input: this does not add tensor computation syntax to `.rr` sources.

`--reussir-prepare-pjrt` packages kernels, removes obsolete source modules, and
strips IFRT function annotations even from hosts without calls. It leaves
ordinary modules unchanged; builds without OpenXLA reject IFRT programs when
preparation is required. For a hand-written `reussir-opt` pipeline, schedule
this pass before host optimization and the normal array, RC, control-flow, and
LLVM lowerings. The `pjrt_call_e2e.mlir` and `pjrt_sharded_e2e.mlir` integration
tests show native-compiler and explicit lowering/linking commands.
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

## Host transfers for sharded arrays

`reussir.array.to_device` distributes a global strided host memref across the
array's ordered devices. `reussir.array.to_host` gathers shards into a dense
row-major host memref. Both operations support concrete IFRT `sharding_param`
metadata, including uneven multidimensional tiles, mesh/device permutations,
replication, dynamic extents, empty arrays, and dimensions smaller than their
shard count. Uploads preserve signed host strides and view offsets; downloads
preserve the destination view offset.

Shard placement follows upstream IFRT's
[`ShardingParam`](https://github.com/openxla/xla/blob/fc840b37e26bd36c99f32e295324a1700dc12ecd/xla/python/ifrt/ir/sharding_param.h)
device permutation. Physical shapes and edge domains follow XLA's
[`HloSharding::TileShape`, `TileOffsetForDevice`, and `TileLimitForDevice`](https://github.com/openxla/xla/blob/fc840b37e26bd36c99f32e295324a1700dc12ecd/xla/hlo/ir/hlo_sharding.cc):
each physical dimension is `ceil(global_extent / shard_count)`, while the valid
logical domain is clipped to the global array. This matches SPMD execution;
IFRT's higher-level `ShardingParamShardingSpec::GetShardShape` still rejects
uneven shapes at this upstream revision and is not used for physical sizing.
Device allocation and transfers use the same ceiling-sized physical tiles.

Uploads zero-fill edge padding and initialize every replica. Downloads select
one representative per tile, assuming replicas contain identical values, and
copy only valid logical rows from temporary row-major shard storage. Entirely
padded devices never access or advance pointers into host storage. Plugins need
not implement strided host downloads. Transfers finish borrowing host storage
before returning, and downloads can initialize newly allocated host arrays.

Unspecified multi-device sharding does not identify which host values belong to
which devices, so transfers require concrete topology. Unreduced axes describe
partial values rather than replicas; `sharding_param` records the axes but not
the reduction operator. Upstream Shardy supports distinct
[SUM, MAX, and MIN unreduced semantics](https://github.com/openxla/xla/blob/fc840b37e26bd36c99f32e295324a1700dc12ecd/xla/service/spmd/shardy/test/sdy_round_trip_export_pipeline.mlir).
Materialize those reductions explicitly before downloading, or upload into a
reduced array before converting to a partial representation. Transfers diagnose
missing reduction semantics rather than discard partial values or assume SUM.
Sharding metadata requires an OpenXLA-enabled compiler; single-device transfers
without sharding metadata remain available without OpenXLA.

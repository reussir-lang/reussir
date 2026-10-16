# Runtime StableHLO specialization

`reussir-stablehlo` is a safe wrapper over `reussir-stablehlo-sys`. Its native
library links the pinned StableHLO/MLIR libraries and the IFRT dialect support
archive (including SDY), without the Reussir compiler or JIT. Each request owns
an independent context. Input can be MLIR text or bytecode; output is verified
MLIR bytecode and concrete logical result dimensions.

Build the native archives first:

```
cmake --preset nix-dev -B build -DREUSSIR_ENABLE_OPENXLA=ON
cmake --build build --target reussir-stablehlo-test -j 4
```

Direct Cargo users set `REUSSIR_STABLEHLO_LIB_DIR` to the build's `lib`
directory and `MLIR_SYS_230_PREFIX` to the same LLVM/MLIR prefix as CMake.
The generated archive manifest and content fingerprints keep Cargo and sccache
aware of native changes. No LLVM dependency is introduced into default workspace
members by these opt-in crates.

Argument dimensions are **logical global** dimensions. They must agree with
static dimensions in the module. Optional `shape_values` replace uses of an
explicit host-known i32/i64 scalar or vector with a constant. This does not read
any device buffer or specialize arbitrary scalar inputs, and retains the input
signature. Callers must ensure the supplied value is the launch's actual value.
Data-dependent dynamism that remains unresolved is an error, not a fallback.

Shape propagation uses the pinned upstream
[`createStablehloRemoveDynamismPipeline`](https://openxla.org/stablehlo/dynamism),
with inlining, CSE, canonicalization and final static-shape validation, following
XLA's `xla/python/refine_polymorphic_shapes.cc`. The library does not enumerate
program variants or make ownership/donation decisions.

The PJRT runtime enables this library only through its `pjrt` feature. Compiler
launches pass `SpecializationArgument` records to
`__reussir_pjrt_compile_specialized`. Dynamic input extents come from the RC
array descriptor. The ABI also supports explicit host-known shape values;
ordinary compiler launches leave them absent. Partitioned dynamic outputs still
require the existing explicit global-extent contract and remain rejected by the
frontend without it.

Executable-cache identities include the original verified bytecode digest,
compile options, logical shapes, and presence and contents of each shape value.
The disk identity also carries a specialization version and the existing
plugin/platform/topology/XLA_FLAGS namespace. Refinement and PJRT compilation
both run inside the coalesced cache miss; a disk hit avoids both.

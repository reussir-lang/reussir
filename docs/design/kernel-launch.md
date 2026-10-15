# Host kernel launches

`core::intrinsic::pjrt::launch<P>(kernel<T>, arguments...)` runs a named `#[kernel]` function on device
buffers. `P` is an optional compile-time configuration type; omitting it selects
`core::intrinsic::pjrt::DefaultPlacement`. Kernel type arguments are independent
of the launch configuration and may be inferred.

```reussir
#[placement(devices = "0", layout = "default")]
#[launch(backend_optimization_level = "2", donation = "none")]
pub struct [value] Config {}

#[kernel]
fn choose(x: Tensor<[f32; 4]>, enabled: bool) -> Tensor<[f32; 4]> {
    if enabled { x } else { x }
}

pub fn run(x: DevArray<[f32; 4]>, enabled: DevArray<bool>)
    -> DevArray<[f32; 4], Config> {
    core::intrinsic::pjrt::launch<Config>(choose, x, enabled)
}
```

The target is a direct kernel reference, including an imported or generic
function. It is not a closure or a first-class function argument. Launch is a
host operation; kernels call other kernels with ordinary call syntax.

## Buffer ABI and ownership

A kernel must return exactly one `Tensor`. The result is a fresh owned
`DevArray` descriptor with the same source element type and global shape, using
configuration `P` for its output placement. Future, Token, scalar, unit, and
product results are not launch results.

A Tensor parameter takes a DevArray of the same element type and global shape.
A numeric or bool scalar parameter takes a rank-zero DevArray of that scalar
type. Tuples are flattened recursively in field order; unit contributes no
buffer. Scalar values are not uploaded implicitly. Unsigned buffers retain
unsigned device element types.

Input configuration markers may differ from `P`. All input and output arrays
must use the same ordered logical device list, while each keeps its own
sharding, memory kind, and layout. Thus a default-placement upload can feed a
custom configuration that selects the same devices. No implicit resharding or
copy between device sets occurs.

Launch borrows its arguments for synchronous execution. Variables remain live
through the call and temporaries are released afterward. The existing ownership
and uniqueness analysis may select donation when `donation = "auto"`; it never
forces donation of a shared live buffer. `donation = "none"` disables donation
variants for that call. Both modes return independently owned descriptors.

## Configuration

Configuration uses the existing zero-field, non-generic `[value]` placement
marker. `#[placement]` specifies ordered devices, sharding, optional memory kind,
and optional layout. A separate `#[launch]` attribute supports:

| Option | Values | Meaning |
| --- | --- | --- |
| `backend_optimization_level` | `"0"`, `"1"`, `"2"`, `"3"` | Sets XLA's backend optimization level in serialized compile options. Omission preserves the backend default. |
| `donation` | `"auto"`, `"none"` | Allows the existing safe donation optimization, or opts out. Default: auto. |
| `view` | `"global"` | Kernel shapes describe logical global tensors. Default: global. |

Unknown, duplicate, malformed, and misplaced options are errors. Local view is
explicitly rejected because its replica and local/global shape rules require a
different source type mapping. Launch options do not change DevArray storage.
The typed configuration survives HIR, MIR, and package-interface roundtrips;
no runtime configuration object is allocated.

## IFRT mapping and supported boundary

The implementation follows the pinned XLA revision
`fc840b37e26bd36c99f32e295324a1700dc12ecd`, specifically
[`ifrt.Call`](https://github.com/openxla/xla/blob/fc840b37e26bd36c99f32e295324a1700dc12ecd/xla/python/ifrt/ir/ifrt_ops.td)
and its
[default compile-option construction](https://github.com/openxla/xla/blob/fc840b37e26bd36c99f32e295324a1700dc12ecd/xla/python/ifrt/ir/transforms/utils.cc).

| IFRT feature | Source launch behavior |
| --- | --- |
| Callee and tensor signature | A private nested module contains the reachable kernel closure and a tensor-only `main` entry. Nongeneric imported kernel bodies are exported too. |
| Input/output arrays | Exact source dtype/global shape, descriptor sharding, ordered devices, memory and layout are retained. |
| Multiple or zero outputs | Supported by IFRT; not exposed by the source's exact-one-Tensor result rule. |
| Device assignment | Derived from P's ordered devices; one replica and one SPMD partition per device. |
| Local view | Supported by IFRT; rejected by this source mapping. |
| Control operands/result | No host token syntax. Calls execute synchronously through the current runtime. |
| Donation hints and aliases | Derived only by safe ownership analysis; explicit unchecked aliases or forced donation are not exposed. |
| Argument/result metadata | Compiler-generated sharding annotations; arbitrary user dictionaries are not accepted. |
| Compile options | Backend optimization is checked and serialized; parameter convention and assignment are compiler-derived. Raw external option overrides, tupled parameters, and portable executables are not exposed. |
| Cache identity | Kernel bytecode and compile options participate in the existing PJRT cache identity; configuration is not erased before compilation. |

Kernel helpers are lowered as kernel SSA, never native host functions. Standard
MLIR inlining and symbol elimination prepare the isolated compilation unit for
PJRT, including helpers that pass Future values. Recursive kernel call graphs
are rejected. The kernel context marker remains attached through final kernel
lowering. An OpenXLA-disabled build reports an explicit launch diagnostic.

The compiler optimization level controls optional target-independent StableHLO optimization and inferred buffer donation. `none` and `tpde` omit these optimizations; `default`, `aggressive`, and `size` enable them. Required kernel preparation and explicit ownership contracts are preserved at every level. The configuration marker’s `backend_optimization_level` independently selects XLA backend optimization.

Partitioned dynamic result dimensions require a logical result-shape contract, which this syntax does not provide. They receive an early diagnostic: padded physical shard shapes cannot recover an uneven logical result extent. Static uneven results and unpartitioned dynamic results retain their existing runtime paths.

`launch` is an ordinary identifier. The special form is selected by the scoped `core::intrinsic::pjrt::launch` declaration (or its module/function import alias), before its kernel-reference argument is elaborated as a value. A local value or ordinary function named `launch` follows normal call resolution. The core prototype is a declaration anchor; it does not expose launch as a first-class function.

# Kernel async blocks

`async { ... }` starts one eligible StableHLO operation inside a `#[kernel]`
function. The result is `core::intrinsic::tensor::Future<Tensor<...>>`;
`future.done()` obtains its tensor result when the core package is loaded.
The member is an ordinary generic `#[kernel]` method in core whose body calls
`core::intrinsic::tensor::async_done(self)`. The free intrinsic also works in
bare compiler sessions without core declarations.
The compiler emits `stablehlo.async_start` and `stablehlo.async_done`. These
operations express a dependency; they do not guarantee concurrent execution.

```reussir
import core::intrinsic::tensor;

#[kernel]
fn begin(x: Tensor<[f32; 8]>, start: i32) -> Future<Tensor<[f32; 3]>> {
    async { tensor::dynamic_slice<Tensor<[f32; 3]>>(x, (start,)) }
}

#[kernel]
pub fn slice(x: Tensor<[f32; 8]>, start: i32) -> Tensor<[f32; 3]> {
    let pending = begin(x, start);
    pending.done()
}
```

The `dynamic_slice<Result>(input, starts)` intrinsic takes exactly one explicit
result Tensor type. Input and result have the same rank and element type. The
result shape must be static; known input dimensions must be at least as large
as the corresponding slice sizes. Empty dimensions are legal. `starts` is a
tuple with one integer per dimension: either source integers or rank-zero
integer Tensors, all sharing one integer type. Rank-zero input uses `()`.
StableHLO clamps start indices to the input bounds. Source integer scalars in
kernels lower to rank-zero tensors; they are not host values.

`slice<Result>(input, starts)` is the static, unit-stride variant. Its start
offsets are a tuple of nonnegative integer literals, checked as signed 64-bit
attributes. They must fit within the input when its dimensions are known.
Offsets are metadata and emit no operations. For example:

```reussir
#[kernel]
pub fn middle(x: Tensor<[f32; 8]>) -> Tensor<[f32; 3]> {
    tensor::async_done(async { tensor::slice<Tensor<[f32; 3]>>(x, (2,)) })
}
```

`collective_permute(input, pairs)` routes a tensor between replicas and returns
the same Tensor type. `pairs` is a tuple of literal `(source, target)` replica
ids, for example `((0, 1), (1, 0))`. Ids are nonnegative signed 64-bit integers;
each source and each target must be unique. The empty tuple `()` routes no
values, so unmatched replicas receive zero tensors. This API uses cross-replica
routing without a channel handle; it does not invent a partition placement.
The configured runtime replica count determines whether the ids are in range.

The async body must return the result of exactly one `slice`, `dynamic_slice`
or `collective_permute` operation.
Tuple construction, projection and local bindings can arrange its operands and
result because they emit no operations. Bind runtime constants and arithmetic outside
the block; static slice offset literals remain inside as attributes. Calls, conditionals, nested async blocks and `async_done` inside the
block are rejected. This restriction follows the pinned StableHLO verifier,
not a general promise that arbitrary kernel blocks can execute asynchronously.

Captures are explicit in HIR and MIR and become async operands and matching
region block arguments. The region is isolated from outer SSA values. Captured
products flatten to tensor leaves; Future, Token and host handles cannot be
captured. Bind an individual tensor field outside the block if its containing
product also carries such a value. No RC allocation or host closure represents
an async block. Generic substitution and imported bodies repeat validation
before lowering, including the exact body result and capture identities.

Future and Token remain kernel SSA types. Private kernel helpers may accept or
return Future; a public PJRT entry must expose only tensor leaves. A host
function cannot create an async block, call these intrinsics, or receive a
Future from a kernel. Token is a side-effect ordering value, not a completion
handle. This feature adds neither host futures nor a host launch API, and does
not impose an extra source linearity rule on Future or Token.

The general Future type still supports several Tensor payloads. The pinned
async operations support exactly one payload, so `async_done` rejects a
multi-payload Future. Builds without OpenXLA can parse, type-check and round-trip
these programs, but report the missing feature when asked to lower kernels.

## Specification and implementation contract

The reference is [StableHLO async_start and async_done](https://openxla.org/stablehlo/spec#async_start),
checked against revision `b9029ac2228a12e176ea20e56f7d3dcb6bf8a019`:
[operation traits](https://github.com/openxla/stablehlo/blob/b9029ac2228a12e176ea20e56f7d3dcb6bf8a019/stablehlo/dialect/StablehloOps.td)
and [verifiers and return-type inference](https://github.com/openxla/stablehlo/blob/b9029ac2228a12e176ea20e56f7d3dcb6bf8a019/stablehlo/dialect/StablehloOps.cpp).
The verifier permits one collective or slice operation plus its return, with
Tensor operands matching region arguments exactly. This PR exposes static/dynamic slice and collective permutation
source intrinsics; the other eligible operations do not
acquire source APIs implicitly. See also the
[existing Future/Token type design](stablehlo-async-types.md).

## Tested backend boundaries

The installed CPU PJRT plugin's [XLA importer](https://github.com/openxla/xla/blob/fc840b37e26bd36c99f32e295324a1700dc12ecd/xla/hlo/translate/mhlo_to_hlo/mlir_hlo_to_hlo.cc) supports only one-operand
collective producers in `async_start`; it rejects slices even though the pinned
StableHLO verifier permits them. It also requires an `async_done` operand to be
defined directly by `async_start`, so Future values passed through helper calls
remain valid compiler/StableHLO IR but are not accepted by this importer.
Codegen always inlines straight-line wrappers containing one intrinsic,
including the ordinary `Future.done` member, before emitting MLIR. This does not
require an optimization level or an extra pass. Arguments are evaluated once in
source order; the callee keeps its own variable and ownership frame. Calls,
control flow, closures, regional functions, and explicit transform anchors are
outside this simple-wrapper rule. More complex kernel helpers still use the
existing MLIR inliner (`reussir-opt --inline --symbol-dce`) before PJRT import.
No second completion intrinsic or method-name special case is introduced.

The StableHLO specification and pinned verifier do not prohibit repeated
`async_done` uses. The current [XLA async contract](https://openxla.org/xla/async_ops)
requires exactly one user for an async-start result, and the CPU PJRT compiler
rejects two completions of the same future. Call `done()` once and reuse the
returned Tensor wherever needed. This backend restriction is not a claim of
StableHLO undefined behavior, and this change adds no source consumption rule.

The execution regression uses
actual collective permutations, ordinary `done()` calls in a while body, and
checks both delivered tensors and zero results for unmatched replicas.

Rank-zero dynamic slice has no indices and is equivalent to rank-zero static
slice. Lowering uses the latter form because the pinned StableHLO custom printer
emits an extra comma for the former. Both remain genuine eligible operations
inside the async region. Source→HIR→MIR→MLIR textual round trips cover this case.

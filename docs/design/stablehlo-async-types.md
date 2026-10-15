# StableHLO async value types

`core::intrinsic::tensor` exposes `Token` and
`Future<Tensor<...>, ...>`. `Token` takes no type arguments. `Future` takes a
nonempty ordered list of tensor types, preserving each payload's scalar kind,
rank, static dimensions, and dynamic dimensions. Generic payload arguments
are checked again after substitution. Scalars, tokens, nested futures, and
host containers are not future payloads.

The core declarations are intrinsic anchors, not runtime records. `Future<T>`
anchors the minimum one payload; its compiler-provided former accepts further
payload arguments. This does not add variadic generic records. Ordinary names
may shadow the prelude names; the qualified intrinsic names and imported
aliases retain builtin identity and distinct mangling.

## Meaning and pinned implementation

The [StableHLO specification](https://openxla.org/stablehlo/spec) defines tokens
as ordering values for side effects and futures as tensor result types.
Tokens may fan out and join with `after_all`. Neither source type imposes
single-use ownership. They are distinct from Reussir allocation-reuse tokens
and IFRT control values.

The compiler pins StableHLO revision
[`b9029ac2228a12e176ea20e56f7d3dcb6bf8a019`](https://github.com/openxla/stablehlo/tree/b9029ac2228a12e176ea20e56f7d3dcb6bf8a019).
Its [type definitions](https://github.com/openxla/stablehlo/blob/b9029ac2228a12e176ea20e56f7d3dcb6bf8a019/stablehlo/dialect/StablehloTypes.td)
store an ordered list of future payloads. Its
[verifiers](https://github.com/openxla/stablehlo/blob/b9029ac2228a12e176ea20e56f7d3dcb6bf8a019/stablehlo/dialect/StablehloOps.cpp)
require ranked tensor payloads but permit an empty list; Reussir follows the
specification's nonempty grammar. The pinned `async_start` and `async_done`
operations support one payload. That operation restriction does not narrow
the future type. StableHLO does not guarantee asynchronous scheduling.

## Compiler boundary

These are scope-confined kernel SSA values, lowered to real `!stablehlo.token`
and `!stablehlo.future` types. They have no RC allocation, host promise,
cancellation, or threading API. Host signatures, records, cells, arrays,
nullable/Arc wrappers, closure captures/results, and FFI cannot carry them;
the restrictions are rechecked after generic instantiation. HIR, MIR, and
package metadata preserve the types without an interface-version change.

Private StableHLO helper functions may carry these values. Public kernel
signatures, including the PJRT entry, remain ranked-tensor buffer interfaces.
OpenXLA-disabled builds parse and serialize the source types but diagnose
attempted MLIR lowering explicitly. This change supplies types and validation;
it does not add source async operations or kernel launch lowering.

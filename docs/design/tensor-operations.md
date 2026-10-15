# Basic tensor operations

These operations run inside `#[kernel]` functions. Tensor values stay in kernel
SSA; allocation, transfers, and launch are separate APIs. The existing async
single-operation body checker still permits its original slice and collective
operations. These new operations do not silently expand that eligibility;
unsupported async bodies receive the existing single-operation diagnostic. The operations follow
[the pinned StableHLO specification](https://github.com/openxla/stablehlo/blob/b9029ac2228a12e176ea20e56f7d3dcb6bf8a019/docs/spec.md).

```rust
import core::intrinsic::tensor;

#[kernel]
pub fn affine(x: Tensor<[f32; 2, 3]>, weights: Tensor<[f32; 3, 4]>)
    -> Tensor<[f32; 2, 4]> {
    let bias: Tensor<[f32; 2, 3]> = tensor::fill(1.0);
    tensor::dot(x + bias, weights)
}
```

## Initialization

| Call | Meaning |
| --- | --- |
| `fill<Result>(value)` | Broadcast a scalar or rank-zero Tensor to the static result shape. |
| `iota<Result>(axis)` | Fill each element with its index along the literal axis. |
| `dynamic_fill<Result>(value, shape)` | Broadcast using runtime dimensions. |
| `dynamic_iota<Result>(shape, axis)` | Iota with runtime dimensions and a literal axis. |

`Result` can be omitted when a binding annotation, return type, or another use
constrains it. The fill value's dtype is inferred from that result, including
through intermediate bindings. Iota requires a numeric result and a literal
`i64` axis in `[0, rank)`. Fill supports every existing Tensor element type,
including bool. Rank-zero and zero-extent fills are valid; rank-zero iota has no
valid axis, while zero extents on a positive-rank tensor are valid.

A runtime shape is a rank-sized tuple of `i64` or `Tensor<i64>` dimensions; `()`
is the rank-zero shape. Literal dimensions are checked for nonnegativity,
signed-64-bit representability, and agreement with known result extents. Runtime
dimensions have the same preconditions. The result rank is always known:

```rust
#[kernel]
pub fn rows(n: Tensor<i64>) -> Tensor<[f32; ?, 4]> {
    tensor::dynamic_fill(2.0, (n, 4))
}
```

Dynamic result types emit `stablehlo.dynamic_broadcast_in_dim` or
`stablehlo.dynamic_iota`. Whether a backend executes a particular dynamic shape
is a backend capability; the frontend does not silently replace dynamic types
with static shapes. CPU execution tests cover dynamic constructors with known
result extents, while verifier tests also cover genuinely dynamic result types.

## Arithmetic and selection

`+`, `-`, `*`, `/`, `%`, unary `-`, and comparisons lower directly to tensor
operations. Free intrinsics `add`, `sub`, `mul`, `div`, `rem`, `neg`, `abs`, `min`,
`max`, `eq`, `ne`, `lt`, `le`, `gt`, and `ge` provide the same operations.

Binary operands have identical shapes and element types. There is no implicit
broadcast, dtype promotion, or conversion. Numeric operations support the
existing signed/unsigned 8/16/32/64-bit integers and floating formats (`f16`,
`bfloat16`, `f32`, `f64`), with each backend's execution support still
applying. The existing Tensor type excludes `float8`. StableHLO boolean add/multiply mean OR/AND; min/max and comparisons
also accept bool. Subtract/divide/remainder/negate/abs require numeric elements.
Integer division truncates toward zero. Floating operations, NaNs, and integer
edge cases follow the corresponding StableHLO operation.

Comparisons return a same-shape `Tensor<bool>`. `select(predicate, yes, no)`
selects elementwise; the predicate is a scalar or same-shape boolean Tensor.
Both values are evaluated. This differs from kernel `if`, which selects a
region with a scalar predicate.

```rust
#[kernel]
pub fn positive(x: Tensor<[f32; 8]>) -> Tensor<[f32; 8]> {
    let zero: Tensor<[f32; 8]> = tensor::fill(0.0);
    tensor::select(x > zero, x, zero)
}
```

## Dot and transpose

`dot(lhs, rhs)` lowers to `stablehlo.dot_general`, with no batching dimensions,
the last left dimension and first right dimension contracting, and default
StableHLO precision. It covers all four vector/matrix combinations:

| Left | Right | Result |
| --- | --- | --- |
| `[T; K]` | `[T; K]` | `Tensor<T>` |
| `[T; M, K]` | `[T; K]` | `Tensor<[T; M]>` |
| `[T; K]` | `[T; K, N]` | `Tensor<[T; N]>` |
| `[T; M, K]` | `[T; K, N]` | `Tensor<[T; M, N]>` |

The input and output element types match and are numeric. Known contracting
extents must agree; dynamic contracting extents must agree at execution. Empty
contracting dimensions and zero output dimensions are allowed. Result shape is
inferred from concrete operand types, with an explicit result type available
for generic calls. Arbitrary batches, contraction axes, quantized/complex
types, and precision algorithms are outside this basic API.

`transpose(matrix)` swaps the two dimensions of a rank-two tensor and infers the
result type. It supports every existing tensor dtype.

## Ordinary core members and generic shapes

The core declaration supplies normal `#[kernel]` methods, such as
`x.add(y)`, `x.dot(y)`, `x.transpose()`, `x.lt(y)`, and `mask.select(x, y)`.
Their bodies call the free intrinsics. `Tensor::iota()` enumerates axis zero;
use the free intrinsic to choose another axis. Constructors can infer their
shape argument from context:

```rust
#[kernel]
pub fn member_example(x: Tensor<[f32; 2, 3]>) -> Tensor<[f32; 2, 3]> {
    let value: f32 = 2.0;
    let filled: Tensor<[f32; 2, 3]> = tensor::Tensor::fill(value);
    let ramp: Tensor<[f32; 2, 3]> = tensor::Tensor::iota();
    x.add(filled).mul(ramp)
}
```

Ordinary generic signatures cannot infer a scalar dtype from a different
shape-type parameter. Annotate scalar values passed to constructor members,
and annotate dimensions of shape tuples passed to dynamic constructor members.
Free intrinsics carry the stronger builtin relation and infer these literal
types directly. Comparison member results may also need an annotation; source
comparison operators and free comparison intrinsics infer their boolean shape.

`Tensor<T>` accepts a complete scalar-or-shape argument: `T = f32` gives rank
zero and `T = [f32; 2, 3]` gives rank two. Generic substitution reapplies this
former, so ordinary `impl<T> Tensor<T>` methods work for every rank.
`Tensor<[T; 2, 3]>` instead fixes the shape and varies only the element type.
Concrete rank-zero tensors never unify with ranked tensors, and nested array
elements remain invalid.

The executable examples are
[`frontend/tensor_operations.rr`](../../tests/integration/frontend/tensor_operations.rr),
[`frontend/tensor_operations_core.rr`](../../tests/integration/frontend/tensor_operations_core.rr),
and [`runtime/tensor_operations.rr`](../../tests/integration/runtime/tensor_operations.rr).
The last checks actual tensor buffers on CPU PJRT, including repeated execution
with a non-donated input.

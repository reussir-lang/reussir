# Kernel functions and conditionals

`#[kernel]` marks a function or method as executing in kernel context.
Kernels can call kernels. Host code can call a kernel only when its concrete
return type is `Tensor`; generic calls are checked again after specialization.
A kernel function cannot become an ordinary closure or host trampoline.
Trait declarations and implementations must agree on the annotation.

Kernel signatures accept tensors, supported numeric and boolean scalars, unit,
and nested tuples of kernel values. Private helpers may also use `Token` and
`Future`; public PJRT entries must flatten to tensor inputs and outputs.
Generic types are checked when instantiated. Host calls, allocation, records,
and closures remain invalid in kernels.

## SSA representation

Kernel lowering has its own environment, separate from host aggregate and
reference-count lowering. A scalar becomes a rank-zero tensor, `Tensor<T>` keeps
its ranked shape, a tuple recursively becomes an ordered list of SSA values,
and unit contributes no values. Unsigned elements use unsigned MLIR integer
types, preserving unsigned division, comparison, and shift semantics. Bindings,
projections, calls, and returns use the same flattening rules. There is no
heap allocation or legacy StableHLO tuple operation for a source tuple.

The `is_kernel` flag survives HIR and MIR serialization, dependency interfaces,
generic specialization, and the final lowering environment. Emitted kernel
functions carry `reussir.kernel_function`. A native compilation attempt diagnoses an
unlaunched kernel before host conversion. Host-to-kernel launching is not yet
implemented; it cannot accidentally become an ordinary host call.

## Conditionals

A kernel `if` accepts a `bool` or a rank-zero `Tensor<bool>` predicate and emits
`stablehlo.if`. Branches capture outer SSA values and return the same flattened
result types. Results may contain tensors and tokens, including nested source
tuples and unit. A `Future` cannot cross an `if` result boundary, matching the
[StableHLO if specification](https://openxla.org/stablehlo/spec#if).

```rust
#[kernel]
pub fn choose(c: bool, n: i64, x: Tensor<[f32; 2]>, y: Tensor<[f32; 2]>)
    -> (Tensor<[f32; 2]>, i64) {
    let selected = if c { x } else { y };
    (selected, if n < 0 { -n } else { n })
}
```

Numeric and boolean literals, arithmetic, comparisons, negation, and bitwise
operations emit StableHLO operations on rank-zero tensors. Short-circuit boolean
expressions retain their conditional evaluation. Ordinary host functions keep
their existing scalar and `scf.if` lowering.

Executable examples cover [textual IR replay](../../tests/integration/frontend/kernel_if_lowering.rr),
[imported generic kernels](../../tests/integration/frontend/kernel_if_import.rr),
and [real CPU PJRT execution](../../tests/integration/runtime/kernel_if.rr).
The PJRT test supplies a standalone entry name and invokes the existing runtime
ABI explicitly; it does not introduce source launch syntax.

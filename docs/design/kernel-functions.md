# Kernel function context

`#[kernel]` marks a function or method as executing in kernel context.
Kernels can call kernels. Host code can call a kernel only when its concrete
return type is `Tensor`; generic calls are checked again after specialization.
A kernel function cannot become an ordinary closure or host trampoline.
Trait declarations and implementations must agree on the annotation.

Kernel signatures accept tensors, supported numeric and boolean scalars, and
unit. Generic types are checked when instantiated. Bindings, blocks, and
conditionals preserve kernel context. Ordinary host operations, host calls,
allocation, and closures are rejected inside kernels.

The `is_kernel` flag survives HIR and MIR serialization, dependency interfaces,
generic specialization, and the final lowering environment. The final `if`
dispatch consults this flag so kernel control flow cannot use host lowering.
This change introduces function identity and boundary checks only: kernel
operations, compilation, and launching report explicit lowering diagnostics
until their implementations are added.

#import "/book.typ": book-page

#show: book-page.with(title: "Trampolines")

== Trampolines

`reussir.trampoline` models the backend FFI boundary for ordinary native calls.
The operation now carries an explicit direction:

- `reussir.trampoline export "C" @ffi_name = @target`
- `reussir.trampoline import "C" @reussir_name = @c_target`

`export` exposes an internal Reussir function to C-facing callers. `import`
defines a Reussir symbol that forwards to a C-facing target already declared in
the module.

=== Trivial Boundary

A function is *trivial* for the backend trampoline when:

- it accepts fewer than four arguments;
- every argument is an integral or pointer LLVM value;
- it returns either no result or exactly one integral or pointer LLVM value.

Trivial trampolines do not need ABI reshaping. The backend therefore lowers
them as direct forwards with best-effort minimal overhead.

=== Nontrivial Boundary

Any function outside the trivial class uses the explicit trampoline boundary
below.

For nontrivial returns, the trampoline always uses an explicit leading return
pointer parameter. The backend does not attach ABI-specific `sret` metadata to
that parameter. This rule is backend-defined and no longer depends on
platform-specific C ABI aggregate classification.

For nontrivial calls, the trampoline packs all logical arguments into a literal
LLVM struct in source order and passes a pointer to that struct. This applies
even when the original arguments are individually integral or pointer values,
for example once the arity reaches four.

The resulting exported boundary therefore has one of these shapes:

- trivial call: direct values in, direct value or `void` out;
- nontrivial return only: `void` return plus leading return pointer;
- nontrivial arguments only: direct return plus one pointer to the packed
  argument struct;
- both nontrivial: leading return pointer plus one pointer to the packed argument
  struct.

=== Import And Export Responsibilities

The backend now focuses only on lowering the trampoline symbol itself.
It does *not* synthesize caller-side stack storage for the packed argument
struct or for the explicit return destination.

That means the caller or callee on the C side is responsible for choosing
whether it passes direct values or explicit pointers according to the trampoline
signature. For `export`, the external C caller must obey that ABI shape. For
`import`, the referenced C target must already be declared with that ABI shape,
and the Reussir-side caller must provide the corresponding arguments.

=== Operation Shape

The operation now includes an import/export enum attribute in assembly syntax:

```mlir
reussir.trampoline export "C" @sum_ffi = @sum_internal
reussir.trampoline import "C" @sum_internal = @sum_c
```

Currently only the `"C"` ABI is supported.

=== TODO

- The frontend should expose user-facing facilities to import `"C"` functions
  through `reussir.trampoline`, including helpers for constructing the packed
  argument struct pointer and any required explicit return storage in
  JIT-facing flows.

# Dynamic-extent arrays: the strided-header box

Extends `!reussir.array` from statically shaped only (#344 Phase A) to
per-dimension dynamic extents (`!reussir.array<? x f64>`), keeping rank
static, exactly as memref spells it. The motivation is ecosystem-shaped:
the array view is already a memref, and once extents can be runtime
values an rc-managed Reussir array is a legal operand for the entire
memref/tensor-consuming MLIR universe — `linalg` on both forms, and the
OpenMP/Async/Enzyme directions named in AGENTS.md all speak memref. The
fixed-shape restriction, not the rc box, is what keeps Reussir arrays a
niche citizen today. Growable buffers are explicitly *not* this feature:
`std::collections::cow::vec` remains the growable story, and nothing here
adds a resize operation.

## Box layout: the header is the memref descriptor

A dynamic array's rc box stores the full strided-memref encoding —
offset, sizes, strides, static dims included — and derives only the two
descriptor pointers (both are `box + header`):

```
{ i32 count | pad to align(index) | index offset
  | index size[0..r] | index stride[0..r] | pad to align(elem) | payload }
```

The header is exactly what `memref.extract_strided_metadata` returns,
minus the base buffer. It is never a hand-computed byte table: the
members `{i32, index, index×r, index×r}` go through `deriveCompoundLayout`
against the module's `dlti.dl_spec`, so index width and padding follow
the target — on a 32-bit target index is 4 bytes and the header has no
padding at all. This is the same discipline that makes `str`'s `{ptr,
len}` lowering take `converter.getIndexType()` and that
`reussir-convert-to-llvm` exists to preserve (its type converter is built
from the stamped `llvm.data_layout`, not upstream's frozen 64-bit
default). The header prefix is static per rank, so `RcBoxType` keeps
answering alignment queries; only the tail size becomes dynamic.

Static arrays keep today's headerless box and identity-layout
`memref<NxT>` view — no regression, and the two forms stay distinct
types.

Dynamic arrays currently support shared boxes only. Regional (`flex`/`rigid`)
RC pointers and references, and regional dynamic-array box types, are rejected
by the type verifiers: the regional state/next/vtable header has no strided
metadata extension yet.

## Views and projection

`getArrayViewMemRefType` for a dynamic array returns
`memref<S0x…xT, strided<[?,…], offset: ?>>`: static dims may stay static
in the shape, but the layout is always the dynamic `StridedLayoutAttr`.
`array.view` lowering becomes a straight header copy — GEP to the payload
for both descriptor pointers, then `2r+1` loads — replacing
`MemRefDescriptor::fromStaticShape`. Uniform for every static/dynamic
mix; no constants-vs-loads case split.

`array.project` lowers in `reussir-convert-to-std` to a non-negative-index
check, an unsigned check against the leading `memref.dim`, and a guarded rank-reducing
`memref.subview`. Out-of-bounds indices panic. Subview inference retains
known strides and the memory space; MLIR value-bounds analysis omits the guard
when it proves `0 <= index < extent`, including induction variables in bounded
loops. Otherwise the panic guard remains. The offset includes `i * stride[0]`.
The final projection bridges a rank-zero subview with
`reussir.ref.from_memref`. This applies equally to static arrays, dynamic
arrays, and other strided memrefs. Before LLVM lowering, the pipeline runs
`expand-strided-metadata` and `lower-affine` to expand the standard view
operations.

The strided layout also buys mutation-free layout ops: transpose,
reverse, and leading-dim slice rewrite the header only, gated by the same
unique-or-clone discipline as `array.with_unique_view` (the header is
part of the CoW'd box). Stride-0 broadcast is excluded from owning boxes:
aliased elements would break the drop traversal's exactly-once contract.

## Invariants

- **Canonical on construction.** Constructors and clones always produce
  offset 0 and row-major suffix-product strides. Non-canonical headers
  arise only from in-place restride of an existing allocation, which
  never changes the footprint — so `token.alloc` size is always
  `header + ∏sizes × elemsize`, computed on the canonical form. The
  touched-extent formula (`offset + Σ(size_i − 1)·stride_i + 1` elements)
  is for verifier/debug assertions only, never allocation.
- **Clone compacts.** The `with_unique_view` clone branch reads dynamic
  extents from the source view and constructs a canonical `array.create`
  with those extents. Its initializer loads each logical source element
  through the strided view and acquires its ownership before yielding it.
  The same path handles static arrays. Sharing never entrenches a degenerate
  layout.
- **Tokens.** `rc.dec` of a dynamic array produces `token<align, ?>`,
  the existing universal fallback donor; `token.alloc` gains an SSA size
  operand (the `__reussir_allocate` entry point already takes a runtime
  size — only the constant-gated `_small` fast path stays static-only).
  Boxes past `kAllocatorBinModelMax` are excluded from reuse pairing,
  implementing the #344 integration note.

## What has to change (audited)

- `ArrayType::verify` and the parser/printer: accept and print `?`
  (`ShapedType::kDynamic`); today any negative extent is rejected.
- `ArrayType::getTypeSizeInBits`: must fail loudly on a dynamic shape
  instead of multiplying the sentinel; audit every caller
  (`TokenInstantiation::getTokenType`, the `ConvertToSTD` clone branch's
  `getTypeSize(rcBoxType).getFixedValue()`, `memberStorageType`).
- `RcBoxType::getTypeSizeInBits`/`getABIAlignment`: the
  `llvm_unreachable("must have a fixed size")` paths route to the
  runtime size computation; alignment is already shape-independent.
- Type converter: nested `LLVM::LLVMArrayType` has no dynamic form, so
  converting a dynamic array as a concrete value fails. Only the dynamic
  box layout uses a header struct plus a trailing zero-length-array-style
  tail.
- `emitArrayElementTraversal` (drop/acquire): loop bounds from header
  loads; the ≤4 unroll threshold gates to static shapes; iteration walks
  the logical index space through strides (exactly-once by the
  no-aliasing invariant).
- Pipeline: insert `expand-strided-metadata` before
  `reussir-convert-to-llvm`. No memref-level pass runs today, and with
  strided layouts first-class, transform-anchor tiling can legitimately
  leave `memref.subview`/`extract_strided_metadata` in the IR.
- Frontend: `TyKind::Array` dims become per-dimension static/dynamic;
  `eval_extent` stops requiring literals (a non-literal extent makes the
  dim dynamic and the value an argument of `splat`/`tabulate`); add an
  `array::dim` intrinsic; lengths stay `u64` until `usize` lands.

## Status

Dialect landed: `!reussir.array<? x T>` parses/verifies (`?` extents,
`ShapedType::kDynamic`), size queries assert on dynamic shapes, and the
type verifiers restrict dynamic arrays to shared boxes; the strided-header
box (`RcBoxType` `hasDynamicArrayPayload` — `{i32 count | index offset |
sizes | strides | tail}`, index-typed so the width follows the target);
`array.view` builds the strided descriptor from header loads (box
recovered from the payload ref by the static header offset); `token.alloc`
takes an SSA byte size for `token<align, ?>`; `array.create extents(…) body { … }`
creates an RC box with an optional single-block initializer region. The region
takes one index per dimension and yields ownership of an element. Like closures,
it lowers through `array.instantiate`, which initializes the canonical header.
Fixed arrays use the same constructor. Initializer regions survive token reuse;
SCCP, canonicalization, and loop-invariant code motion then run before expansion. An invariant yield with only invariant
ownership acquisitions becomes `array.fill_pattern`; acquisitions are batched
by the element count and skipped for empty arrays. Other bodies become loops.
The fill operation is retained until basic lowering emits
`llvm.experimental.memset.pattern` with a typed scalar or pointer pattern.
An initializer yielding `ub.poison` still writes the payload: poison does not
mean "leave storage untouched". Destination-passing kernels that reuse an
input buffer can omit the `array.create` body to initialize only the header,
then read each input element before overwriting its slot. Token instantiation
and reuse still apply to this form. The caller must initialize the entire
payload before exposing or releasing the result.
Its `TokenAcceptor::buildTokenSize` implementation computes
`header + product(sizes) * elemsize`;
`rc.dec`/`rc.reinterpret` produce dynamic
tokens freed unsized. `array.project` now lowers through checked strided
subviews. Ownership acquisition and release use `memref.dim` for dynamic
loop bounds; only small static shapes are unrolled. Empty dimensions skip
all element ownership operations. The `with_unique_view` clone branch reads
dynamic extents with `memref.dim` and uses the `array.create` sizing interface
to allocate the clone. Its initializer copies logical elements through the
source view, retaining managed elements and constructing a canonical layout.
Executable e2e: `dynamic_array_e2e.mlir`, `array_project_e2e.mlir`,
`dynamic_array_clone_e2e.mlir`, `dynamic_array_managed_e2e.mlir`, and
`dynamic_array_create_rc_e2e.mlir`.
Open backend halves: restride ops, and wiring the frontend
codegen off its `err(…)`
stubs.

Frontend landed: `?` extents, the `DYNAMIC_EXTENT` sentinel through the
type system, leading runtime extents on `splat`/`tabulate`, `array::dim`,
display/mangling/textual-IR spellings, and `err(…)` stubs at every
lowering path (`--emit mlir` reports "do not lower yet"). The
strided-header box and the descriptor lowering are the open backend half.

## Deferred

- Const-generic extents (the monomorphization half of #344 Phase C)
  produce *static* arrays and stay orthogonal; do not couple.
- Borrowed subviews that outlive an expression re-raise the `str`
  lifescope split (`docs/design/str.md`); owner-level ops ship first.
- A growable/resizable array is a different feature with a different
  contract (`token.realloc` resizes dead donors only, #362); out of
  scope here.

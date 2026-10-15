# Tuple values and destructuring

A tuple is an ordered, structurally typed, inline aggregate. It groups values
without declaring a named record or allocating a new reference-counted box.
Its field types, order, and arity determine its type. Fields may themselves be
tuples, records, arrays, cells, or closures, subject to their existing storage
and capability rules.

## Constructing and returning tuples

```reussir
fn measurements(n: u64) -> (u64, (u64,), ()) {
    (n, (n + 1,), ())
}

pub fn total(n: u64) -> u64 {
    let (first, (second,), ()) = measurements(n);
    first + second
}
```

`()` is the existing unit value and type. `(x)` groups an expression, while
`(x,)` constructs a one-field tuple. The same comma distinction applies to
`(T)` and `(T,)` in type positions. A tuple can contain unit-valued fields;
those fields retain their source indices even though they occupy no useful
payload space. Numeric projections start at zero: `pair.0`, `pair.1`, and
`nested.1.0`. Tuple fields are immutable; replace a tuple value rather than
assigning through a position.

Tuple elements evaluate in source order. A destructuring `let` evaluates its
right-hand side once, then binds the selected fields. Patterns can nest and
use `_` to ignore a field. A single annotation describes the complete pattern:

```reussir
let (first, (second, _)): (u64, (bool, u64)) = (7, (true, 9));
```

Bindings in one destructuring pattern must have distinct names. A `let`
pattern must be irrefutable; use `match` for literal and variant patterns.

## Closures and function parameters

Function parameter declarations remain explicitly named. A tuple parameter
is written `fn consume(pair: (u64, u64))`. Closure arrow types preserve their
existing parameter-list syntax:

```reussir
fn apply_two(f: (u64, u64) -> u64) -> u64 {
    f(2, 3)
}

fn apply_pair(f: ((u64, u64),) -> u64) -> u64 {
    f((2, 3))
}
```

The first closure takes two arguments. The second takes one tuple argument.
Tuples can be captured, passed through partial application, and returned by
closures. Captured tuples follow ordinary closure cloning and ownership rules.
Unit parameters still count toward source arity and partial application, but
occupy no physical function or closure argument slot. Their argument expressions
still execute once in source order, including effects before and after a unit
argument.

## Matching

Tuple patterns compose with literals, variant patterns, nested tuples, guards,
and wildcards. Coverage is checked across the complete product of fields.

```reussir
fn classify(pair: (bool, bool)) -> u64 {
    match pair {
        (true, _) => 1,
        (_, true) => 2,
        (false, false) => 3
    }
}
```

Removing the last arm leaves a non-exhaustive match. Guards do not establish
unconditional coverage: after a failed guard, matching continues with later
arms against the same scrutinee. Field bindings retain the normal ownership
rules throughout that retry.

## Ownership and storage

Tuples use the existing value-aggregate representation. Acquiring or dropping
a tuple recursively acquires or drops its managed fields. Copying a tuple that
contains a shared record keeps the record alive; discarding a field releases
its owned reference. Projecting one field from a temporary releases its other
fields. A tuple itself adds no RC allocation.

A tuple can be a field of a shared or value record, and can contain either
record kind. Inline recursive layouts remain invalid, including cycles hidden
inside tuples, such as a value record with a `(Self,)` field. A boxed link breaks
such a cycle. Tuple structural `Sync` checks each element using value-aggregate
rules; surrounding atomic coloring does not implicitly promote tuple fields.

`Cell<T>` and `RefCell<T>` can store tuples. Reads acquire managed fields,
replacement drops the previous payload, and a loaded tuple may outlive the
cell. Tuples may also contain arrays, and an array fold can use a tuple
accumulator. The existing array element policy still excludes inline tuple
**elements**. `Atomic<T>` requires a supported primitive; lock-guarded cells
require a supported scalar or pointer slot. A tuple itself is neither an
`Arc` payload box nor a nullable pointer.

Unfrozen regional values cannot escape into tuple storage. Kernel-only SSA
values retain their scope restrictions when nested in tuples. These rules do
not introduce new region or kernel execution operations.

Native C trampolines use the established packed-aggregate argument and return
ABI. Polymorphic Rust FFI signatures currently reject inline tuple arguments and
results, as they do value records; a tuple does not acquire a new Rust ABI.

## Serialization and examples

Tuple types and expressions survive HIR and MIR textual round trips. Package
interfaces preserve tuple fields and generic tuple bodies, allowing a consumer
to instantiate imported functions without reconstructing a nominal record.

The executable examples are also regression tests:

- [tuple_values_e2e.rr](../../tests/integration/frontend/tuple_values_e2e.rr)
  demonstrates returned, singleton, nested, and unit tuples; evaluation order;
  managed ownership; record fields; cells; closures; arrays; tuple matches;
  and native aggregate passing, at ordinary and aggressive optimization.
- [tuple_import.rr](../../tests/integration/frontend/tuple_import.rr) exercises
  a separately serialized generic dependency and imported managed records.
- [tuple_patterns.rr](../../tests/integration/frontend/tuple_patterns.rr) checks
  exhaustiveness, guards, unreachable arms, arity, and duplicate bindings.
- The `tuple_*_error*.rr` fixtures check storage constraints, generic bounds,
  inline recursive layouts, and the polymorphic FFI boundary.

- [tuple_values.repl](../../tests/integration/repl-rs/tuple_values.repl)
  demonstrates structural display, persistent destructuring, typed bindings,
  recovery after invalid patterns, and managed values across rebinding in the REPL.

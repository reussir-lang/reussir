# While expressions

A while expression carries explicitly named state and returns its final value:

```reussir
let (count, total) = while [count = 0, total = 0] (count < 10) {
    (count + 1, total + count)
};
```

Initializers run once, left to right, in the surrounding scope. No state binder
is visible to another initializer. State binders shadow surrounding names only
inside the condition and body; duplicate names are rejected. A trailing comma
in the binding list is allowed.

The condition runs before every iteration, including the first, and must have
type `bool`. When it is false the current state is returned. The body's final
expression supplies the entire next state simultaneously. There is no `yield`
keyword, nor a break/continue statement model.

Zero bindings carry and return unit; one binding carries and returns its type;
two or more bindings carry and return a tuple in declaration order. A single
binding can itself contain a nested tuple, including unit fields. The body must
return exactly that state type. For example, swapping two values requires
`(b, a)`, with both operands reading the current iteration's state.

Host loops lower to `scf.while`. The loop owns its state: its condition borrows
that ownership, its body consumes the old state and produces the new one, and
exit transfers the final state to the surrounding expression. Outer values
referenced by the condition or body stay live across every iteration and are
released after the loop when no longer needed. Managed state therefore follows
the same explicit retain/release rules as function arguments and returns.

Kernel loops lower to `stablehlo.while`, following the
[StableHLO specification](https://openxla.org/stablehlo/spec#while).
Source tuple state is recursively flattened into SSA values, and unit contributes
no operands. Condition arguments, body arguments/results, and loop results have
the same flattened types; the condition returns one rank-zero boolean tensor.
Unresolved async futures are not loop state: await them before carrying their
resolved tensor values through the loop.

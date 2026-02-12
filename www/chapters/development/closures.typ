#import "/book.typ": book-page
#import "@preview/cetz:0.4.0" as cetz: draw
#import "@preview/cetz-plot:0.1.2" as cetz-plot: smartart
#import "@preview/fletcher:0.5.3" as fletcher: diagram, edge, node

#show: book-page.with(title: "Closures")

== Closures

=== Overview

Closures in Reussir are `Rc` managed objects.

A special feature of closures is that the underlying `ClosureBox` can of
variable size, depending on the number of arguments. This will lead to certain
challenges in the runtime, as its cleanup routine needs to know the exact layout
so that the cleanup routine can properly clean up the memory. That's why closures
are always wrapped in `Rc` and accompanied by a vtable.

In some sense, a closure is always used linearly. Each application of a closure
consumes the closure. This means that, on each application, we first check if the
closure is uniquely owned. If it is, we proceed directly. Otherwise, we will clone
the closure first.

Only plain values and `Rc` managed objects (shared or rigid) can be captured by
a closure.

=== Runtime Layout

#figure(
  cetz.canvas({
    import cetz.draw: *

    // Define uniform dimensions
    let row-height = 0.6
    let rc-width = 3
    let box-width = 6.5
    let vtable-width = 3.5
    let padding = 0.3

    // RC Pointer
    rect((0, -row-height / 2), (rc-width, row-height / 2), fill: white, name: "rc")
    content((rc-width / 2, 0), [*Rc<Closure>*])

    // RcBox container (outer box)
    let rcbox-x = 5
    let box-height = row-height * 4
    let rcbox-height = box-height + padding * 2 + row-height
    let rcbox-width = box-width + padding * 2

    // RcBox outline
    rect(
      (rcbox-x, -rcbox-height / 2),
      (rcbox-x + rcbox-width, rcbox-height / 2),
      fill: white,
      stroke: (paint: gray, thickness: 1.5pt, dash: "dashed"),
      name: "rcbox",
    )

    // RcBox label
    content(
      (rcbox-x + rcbox-width / 2, rcbox-height / 2 - row-height / 3),
      text(size: 8pt, fill: gray, [RcBox]),
      anchor: "center",
    )

    // ClosureBox structure (inner box)
    let box-x = rcbox-x + padding
    let box-y-offset = -row-height / 2

    // Box outline
    rect(
      (box-x, box-y-offset - box-height / 2),
      (box-x + box-width, box-y-offset + box-height / 2),
      fill: white,
      name: "box",
    )

    // Header
    content((box-x + box-width / 2, box-y-offset + box-height / 2 - row-height / 2), [*ClosureBox*], anchor: "center")
    line(
      (box-x, box-y-offset + box-height / 2 - row-height),
      (box-x + box-width, box-y-offset + box-height / 2 - row-height),
    )

    // Field 1: void* vtable
    content(
      (box-x + box-width / 2, box-y-offset + box-height / 2 - row-height * 1.5),
      [`void* vtable`],
      anchor: "center",
    )
    line(
      (box-x, box-y-offset + box-height / 2 - row-height * 2),
      (box-x + box-width, box-y-offset + box-height / 2 - row-height * 2),
    )

    // Field 2: void* arg_cursor
    content(
      (box-x + box-width / 2, box-y-offset + box-height / 2 - row-height * 2.5),
      [`void* arg_cursor`],
      anchor: "center",
    )
    line(
      (box-x, box-y-offset + box-height / 2 - row-height * 3),
      (box-x + box-width, box-y-offset + box-height / 2 - row-height * 3),
    )

    // Field 3: PayloadTypes... payload
    content(
      (box-x + box-width / 2, box-y-offset + box-height / 2 - row-height * 3.5),
      [`PayloadTypes... payload`],
      anchor: "center",
    )

    // VTable structure
    let vtable-x = rcbox-x + rcbox-width + 1.5
    let vtable-height = row-height * 4

    // VTable outline
    rect((vtable-x, -vtable-height / 2), (vtable-x + vtable-width, vtable-height / 2), fill: white, name: "vtable")

    // Header
    content((vtable-x + vtable-width / 2, vtable-height / 2 - row-height / 2), [*VTable*], anchor: "center")
    line((vtable-x, vtable-height / 2 - row-height), (vtable-x + vtable-width, vtable-height / 2 - row-height))

    // Function 1: drop()
    content((vtable-x + vtable-width / 2, vtable-height / 2 - row-height * 1.5), [`fn drop()`], anchor: "center")
    line((vtable-x, vtable-height / 2 - row-height * 2), (vtable-x + vtable-width, vtable-height / 2 - row-height * 2))

    // Function 2: clone()
    content((vtable-x + vtable-width / 2, vtable-height / 2 - row-height * 2.5), [`fn clone()`], anchor: "center")
    line((vtable-x, vtable-height / 2 - row-height * 3), (vtable-x + vtable-width, vtable-height / 2 - row-height * 3))

    // Function 3: evaluate()
    content((vtable-x + vtable-width / 2, vtable-height / 2 - row-height * 3.5), [`fn evaluate()`], anchor: "center")

    // Arrows
    line((rc-width, 0), (rcbox-x, 0), mark: (end: ">"), name: "arrow1")
    content((rc-width + (rcbox-x - rc-width) / 2, 0.4), text(size: 9pt, [points to]), anchor: "south")

    line(
      (box-x + box-width, box-y-offset + row-height / 2),
      (vtable-x, row-height / 2),
      mark: (end: ">"),
      name: "arrow2",
    )
    content(
      (box-x + box-width + (vtable-x - box-x - box-width) / 2, row-height / 2 + 0.3),
      text(size: 9pt, [vtable]),
      anchor: "south",
    )
  }),
  caption: [Closure runtime layout with reference counting and vtable],
)

The closure structure consists of three main components:

- *Rc<Closure>*: A reference-counted pointer that manages the lifetime of the closure.
- *ClosureBox*: The actual closure data containing:
  - `vtable`: A pointer to the virtual function table
  - `arg_cursor`: A pointer for tracking arguments
  - `payload`: The captured environment variables
- *VTable*: Contains function pointers for:
  - `drop()`: Cleanup and deallocation
  - `clone()`: Creating a copy of the closure
  - `evaluate()`: Executing the closure

=== Construction

In the backend IR, the closure construction is implemented as a `reussir.closure.create` operation.
The operation is basically an isolated region of the function body. The `reussir.closure.create` operation
only creates the closure box without capturing any initial environment. It is the frontend's responsibility to
analyze the initial capture list and use `reussir.closure.apply` operations to supply them. The region body must
have an argument list that matches the closure argument list.

To see this flow in action, consider the following MLIR excerpts.

```mlir
reussir.func private @capture_one() -> !reussir.rc<!reussir.closure<() -> i32>> {
  %closure = reussir.closure.create -> !reussir.rc<!reussir.closure<(i32) -> i32>> {
    body {
      ^bb0(%v0 : i32):
        %one = arith.constant 1 : i32
        %add = arith.addi %v0, %one : i32
        reussir.closure.yield %add : i32
    }
  }
  %captured = arith.constant 5 : i32
  %curried = reussir.closure.apply (%captured : i32)
              to (%closure : !reussir.rc<!reussir.closure<(i32) -> i32>>) :
              !reussir.rc<!reussir.closure<() -> i32>>
  return %curried : !reussir.rc<!reussir.closure<() -> i32>>
}
```

The `closure.apply` call stores the captured integer inside the payload area of the `ClosureBox` and returns a new
handle whose type reflects that no further arguments are required before evaluation.

```mlir
%adder = reussir.closure.create -> !reussir.rc<!reussir.closure<(i32, i64, i32) -> i64>> {
  body {
    ^bb0(%base : i32, %delta : i64, %bias : i32):
      %base64 = arith.extsi %base : i32 to i64
      %bias64 = arith.extsi %bias : i32 to i64
      %sum = arith.addi %base64, %delta : i64
      %result = arith.addi %sum, %bias64 : i64
      reussir.closure.yield %result : i64
  }
}
%captured_base = arith.constant 1 : i32
%captured_delta = arith.constant 2 : i64
%curried = reussir.closure.apply (%captured_base : i32)
           to (%adder : !reussir.rc<!reussir.closure<(i32, i64, i32) -> i64>>) :
           !reussir.rc<!reussir.closure<(i64, i32) -> i64>>
%with_env = reussir.closure.apply (%captured_delta : i64)
            to (%curried : !reussir.rc<!reussir.closure<(i64, i32) -> i64>>) :
            !reussir.rc<!reussir.closure<(i32) -> i64>>
```

The first application installs the `base` value, the second installs the `delta`, and the resulting closure is a
one-argument closure that can be stored, passed around, or evaluated whenever the final `bias` argument becomes
available. Once the arity reaches zero, the runtime uses the vtable entry to enter the `ClosureBox` body and
produce the final result.

=== Application and Evaluation

As mentioned earlier, a closure is always used linearly. Each application of a closure
consumes the closure. This means that, on each application, we first check if the
closure is uniquely owned. If it is, we proceed directly. Otherwise, we will clone
the closure first. This is handled by the `reussir.closure.uniqify` operation.
The `reussir.closure.apply` operation itself does not check if the closure is uniquely owned.
The frontend should always use `reussir.closure.uniqify` to ensure the closure is uniquely owned.
Additionally, all supplied arguments are also "consumed", making it necessary for
the frontend compiler to maintain their ownership properly.

The `reussir.closure.uniqify` operation does not decrease the reference count
of the original closure. Hence, a decrement operation should be performed when the
original closure is no longer needed.

For implementation details, the `reussir.closure.apply` operation basically first
aligns the cursor to the type of the supplied argument and then stores the argument
at the (ptr + aligned cursor) address. Then, it bumps the cursor by the size of the
supplied argument.

=== Evaluation

As for the backend IR, application and evaluation are separated operations: the
former is to provide arguments to an incomplete closure, while the latter is to
compute the output of the closure.

The evaluation is done by the `reussir.closure.eval` operation. Similar to
`reussir.closure.apply`, it also consumes the closure without checking, so
uniqueness check should be performed before the evaluation.

=== Compilation Strategy

It is rather complicated to compile closures. The overall procedure is as follows:

1. the frontend compiler emits closure code without `token` and `vtable` attributes.
2. in the `reussir-token-instantiation` pass, the token is instantiated.
3. in the `reussir-closure-outlining` pass, the closure body is outlined and a proper `vtable` is generated.
4. in the `reuse-analysis` pass (TODO), the closure token is optimized in the same way as other `Rc` managed objects.
5. in the `basic-ops-lowering` pass, remaining closure code is lowered to LLVM IR.

=== Considerations and Remarks

- Unlike `Lean` or `Koka`, argument types are kept within backend IR. We do not
  rely on the so-called "uniformed calling convention" in `Lean`; instead, we keep
  explicit types to simplify
  the application and evaluation logic. In this way, we can avoid boxing and unboxing
  of arguments.

- Closures are special `Rc` objects that can only be created by the `reussir.closure.create` operation.
  However, its memory resource (`token`) is still handled in the same way as other `Rc` managed objects,
  which is different from practices in other languages.
  Closure object cleanup also aligns with other `Rc` managed objects. The backend takes care of the
  destruction logic.

=== Future Work

- Add escape analysis and avoid allocations when possible. Heap memory tokens can possibly be eliminated by using stack memory.
- Consider reusing the closure token if its layout information can be inferred.

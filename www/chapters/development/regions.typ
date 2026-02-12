#import "/book.typ": book-page
#import "@preview/cetz:0.4.0" as cetz: draw
#import "@preview/cetz-plot:0.1.2" as cetz-plot: smartart
#import "@preview/fletcher:0.5.3" as fletcher: diagram, edge, node

#show: book-page.with(title: "Regions")

== Regions

=== Overview

Regions provide Reussir with a mechanism to manage local mutability and cyclic
references. They are particularly useful for data structures whose construction
and mutation are confined within a local region, after which the structure is
safely shared and used immutably outside that region.

For example, the following code demonstrates the construction of a single loop-back
doubly linked list:
```rust
struct DLList {
    [field] prev: DLList,
    [field] next: DLList,
    data: f64,
}

fn test() -> DLList {
  region {
    let list = DLList {  // underlying object is rc<!DLList, flex>
      prev: None,
      next: None,
      data: 0.0,
    };
    list.prev = Some(list);
    list.next = Some(list);
    list
  } // underlying object is rc<!DLList, rigid>
}
```

Once an object is moved out of a region, it is frozen to immutable state. If one
wants to write helper functions for construction and mutation, such functions
can be annotated with the `region` keyword to indicate that the function is
confined to the local region.

```rust
region fn push_back(list: [flex] DLList, data: f64) {
  list.next = Some(DLList {
    prev: Some(list),
    next: None,
    data: data,
  });
}
```

=== Frame-segmented Regions

Regions cannot be directly nested. However, if one really wants to nest regions,
one can call another normal function. Inside that function, one can create another
region. Notice, however, `flex` capability values cannot be passed into nor returned
from normal functions. This is expected as we must separate objects from different regions.

=== Memory Management

The memory management algorithm is based on #link("https://dl.acm.org/doi/10.1145/3652024.3665507")["Reference Counting Deeply Immutable Data
  Structures with Cycles: an Intellectual Abstract"] by Matthew Parkinson.

The region is basically an arena, where objects are only registered but not recycled
during mutation. When the region ends, the `freeze` operation will do a mark-sweep
to collect objects into multiple strongly connected components (SCCs). Each SCC is
then managed by reference counting.

=== Runtime Support

The Reussir runtime provides a sophisticated region-based memory management system
implemented in Rust. The runtime exposes four key extern C functions that manage
the lifecycle of regional objects:

==== Core Runtime Functions

*`__reussir_freeze_flex_object(ptr: *mut u8) -> *mut u8`*

Converts a flexible (mutable) object to a rigid (immutable, reference-counted) object.
This function is called when an object escapes a region. The freeze operation:

- Traverses the entire reachable object graph from the given pointer
- Converts unmarked objects to rank-1 reference-counted objects
- Handles cycles using a union-find data structure to identify strongly connected components
- Increments reference counts for objects already frozen
- Returns the same pointer, now pointing to a frozen object

The freeze operation ensures that all reachable objects within a region are properly
converted to a shared, immutable state before being used outside the region.

*`__reussir_cleanup_region(ptr: *mut u8)`*

Cleans up all unmarked objects in a region. This function is called at the end of a
region scope. It:

- Iterates through the region's linked list of allocated objects
- Deallocates any objects that remain in the `Unmarked` state (not frozen)
- Calls custom drop functions if defined in the object's vtable
- Frees the memory allocated for each unmarked object

Only objects that were not frozen (via `freeze_flex_object`) are cleaned up, as
frozen objects are now managed by reference counting.

*`__reussir_acquire_rigid_object(ptr: *mut u8)`*

Increments the reference count of a rigid object. This operation:

- Uses path compression to find the root of the object's union-find structure
- Atomically increments the reference count in the object's header
- Is called when creating a new reference to an existing rigid object

*`__reussir_release_rigid_object(ptr: *mut u8)`*

Decrements the reference count of a rigid object and disposes it if the count
reaches zero. This operation:

- Finds the root of the object's equivalence class
- Decrements the reference count
- If the count reaches zero, triggers the dispose operation

==== Memory Layout

Each regional object has a header prepended to the actual object data:

```c
struct Header {
    PackedStatus status;  // Unmarked, Rank, Rc, Disposing, or Parent pointer
    Header* next;         // Next object in region's allocation list
    VTable* vtable;       // Pointer to virtual function table
};

struct VTable {
    void (*drop)(void*);           // Optional destructor
    PackedInstr* scan_instrs;      // Scan instructions for GC traversal
    size_t size;                   // Object size in bytes
    size_t alignment;              // Object alignment requirement
};
```

The `status` field uses a packed representation that can encode five different states:
- *Unmarked* (0b00): Object exists in a region but hasn't been frozen
- *Rank* (0b10): Temporary state during freeze, stores union-find rank
- *Rc* (0b11): Reference count for frozen objects
- *Disposing* (0b01): Object is being deallocated
- *Parent*: Pointer to parent in union-find structure (positive address)

==== Scan Instructions

The runtime uses a compact instruction set to traverse object graphs. These instructions
are stored in the vtable and interpreted by the scanner:

- *Field*: Current position contains a pointer to another regional object
- *Variant*: Current position contains a variant tag; jump based on tag value
- *Advance(n)*: Move cursor forward by n bytes
- *Jump(n)*: Skip n instructions in the scan program
- *End*: Scanning complete

Example scan instructions for a doubly-linked list node:

```rust
[
    Instruction::Field,                          // Scan 'prev' field
    Instruction::Advance(offset_of!(next)),      // Move to 'next' field
    Instruction::Field,                          // Scan 'next' field
    Instruction::End,                            // Done
]
```

For variant types, the scanner uses conditional jumps:

```rust
[
    Instruction::Variant,              // Read tag at current position
    Instruction::Jump(3),              // If tag=0, jump to Foo variant
    Instruction::Jump(5),              // If tag=1, jump to Bar variant
    Instruction::End,                  // If tag=2, no fields to scan
    // Foo variant
    Instruction::Advance(offset),
    Instruction::Field,
    Instruction::End,
    // Bar variant
    Instruction::Advance(offset),
    Instruction::Field,
    Instruction::Advance(offset),
    Instruction::Field,
    Instruction::End,
]
```

==== Disposal Algorithm

When a rigid object's reference count reaches zero, the runtime executes a
sophisticated disposal algorithm that handles cyclic references:

1. *Mark Phase*: Starting from the root, perform DFS to mark all reachable nodes as `Disposing`
2. *Reference Decrement*: For each edge in the graph, decrement the target's reference count
3. *Cycle Detection*: Use union-find to identify strongly connected components (cycles)
4. *Cleanup*: Once all reachable objects are marked, call drop functions and deallocate

This algorithm correctly handles complex cyclic structures without leaking memory,
using a combination of DFS traversal and cycle detection through the existing
union-find structure created during freeze.

==== Path Compression

The runtime uses path compression in the union-find structure to optimize repeated
queries. When finding the root of an object, all intermediate nodes are updated to
point directly to the root, reducing future lookup times from O(log n) to nearly O(1).

==== Stack Safety

The freeze operation uses the `stacker` crate to dynamically grow the stack when
needed, preventing stack overflow on deeply nested object graphs. A red zone of
4KB and bump size of 32KB ensure safe recursion even on large data structures.

=== IR and Backend Conversion

The region abstraction is represented in the backend IR using several specialized operations.
The core construct is `reussir.region.run`, which creates a new region scope and manages
the lifecycle of objects allocated within it.

==== Basic Structure

Consider the following MLIR example that creates a simple node structure within a region:

```mlir
!node = !reussir.record<compound "Node" {
  [field] !reussir.record<compound "Node">,
  i64,
  [field] !reussir.record<compound "Node">
}>

reussir.func @test() {
  %res = reussir.region.run -> !reussir.rc<!node rigid> {
    ^bb0(%reg: !reussir.region):
    %token = reussir.token.alloc : !reussir.token<align: 8, size: 48>
    %c42 = arith.constant 42 : i64
    %null = reussir.nullable.create : !reussir.nullable<!reussir.rc<!node flex>>
    %rec = reussir.record.compound (
      %null, %c42, %null :
        !reussir.nullable<!reussir.rc<!node flex>>, i64, !reussir.nullable<!reussir.rc<!node flex>>
    ) : !node
    %rc = reussir.rc.create
      value(%rec : !node)
      token(%token : !reussir.token<align: 8, size: 48>)
      region(%reg : !reussir.region) : !reussir.rc<!node flex>
    reussir.region.yield %rc : !reussir.rc<!node flex>
  }
  reussir.rc.dec (%res : !reussir.rc<!node rigid>)
  return
}
```

==== The Region Handle

The `reussir.region.run` operation takes a single-block region as its body. Crucially, this
block must have exactly one argument of type `!reussir.region`. This region handle serves
as an explicit token that tracks which region objects belong to:

```mlir
%res = reussir.region.run -> !reussir.rc<!node rigid> {
  ^bb0(%reg: !reussir.region):
  // Region handle %reg is used to create objects
  ...
}
```

When creating reference-counted objects within a region, the `reussir.rc.create` operation
requires the region handle as an explicit parameter:

```mlir
%rc = reussir.rc.create
  value(%rec : !node)
  token(%token : !reussir.token<align: 8, size: 48>)
  region(%reg : !reussir.region) : !reussir.rc<!node flex>
```

This ensures that every object is properly registered with its owning region for later
cleanup and freeze operations.

As normal `rc` operations, the `token` is not required in frontend generated
code. The backend will instantiate a token during lowering.

==== Capability Transitions

Notice the type transformation that occurs at the region boundary:

- *Inside the region*: Objects have type `!reussir.rc<!node flex>` (flexible, mutable)
- *Outside the region*: The result has type `!reussir.rc<!node rigid>` (rigid, immutable)

The `reussir.region.yield` operation marks which objects should be frozen and returned:

```mlir
reussir.region.yield %rc : !reussir.rc<!node flex>
```

When `region.run` completes, the runtime automatically:
1. Freezes the yielded object using `__reussir_freeze_flex_object`
2. Cleans up unmarked objects using `__reussir_cleanup_region`
3. Returns the frozen object with `rigid` capability

==== Frontend Implications

The region handle is a low-level implementation detail that should remain hidden from
the high-level language. The frontend compiler must maintain the current region environment
implicitly:

*Hidden Region Context*

From the programmer's perspective, the region is purely syntactic:

```rust
region {
  let node = Node { ... };  // Frontend knows this is in a region
  node
}
```

The frontend tracks that it's inside a region scope and automatically:
- Threads the `!reussir.region` handle through all allocations
- Ensures all `reussir.rc.create` operations receive the correct region handle
- Inserts appropriate `reussir.region.yield` at scope exit

*Region Functions*

For functions marked with the `region` keyword, the frontend adds a hidden first parameter:

```rust
region fn helper(list: [flex] List) {
  // User code
}
```

Lowered to IR:

```mlir
reussir.func private @helper(%reg: !reussir.region, %list: !reussir.rc<!List flex>) {
  // The hidden %reg parameter is threaded through
  ...
}
```

This allows region functions to allocate new objects in the caller's region while keeping
the region handle abstraction invisible to the programmer. The frontend must:

1. Add `!reussir.region` as the first parameter to region function signatures
2. Pass the current region handle when calling region functions
3. Use the region handle for any `reussir.rc.create` operations within the function

*Region Environment Stack*

The frontend maintains a stack of region environments:
- Entering a `region` block pushes a new environment
- Calling a region function inherits the current environment
- Exiting a region pops the environment

This ensures proper scoping and prevents region handles from escaping their valid lifetime.

==== VTable Generation and Backend Conversion

Each type used in a region requires a vtable that describes how to scan its fields.
The `reussir.region.vtable` operation declares these vtables:

```mlir
reussir.region.vtable @vtable1 {
  type(i32)
  drop(@drop_func)
}
```

Importantly, the frontend does not concern itself with vtable generation. Instead, the
backend automatically derives scan instructions from type information during lowering.
For example, the backend uses methods like `RecordType::emitScannerInstructions` to
traverse the type structure and generate the appropriate instruction sequence:

- For record types, it walks each field and emits `Field` instructions for reference-counted
  fields, with `Advance` instructions to skip primitive fields
- For variant types, it generates a `Variant` instruction followed by jump tables to
  handle each case
- The backend calculates field offsets automatically based on type layout

This approach keeps the frontend simple and ensures that vtables are always consistent
with the actual memory layout determined by the backend. The vtables are then materialized
as global constants in the LLVM IR, allowing the runtime to traverse object graphs during
freeze and disposal operations.

When objects are created in a region, the backend automatically inserts the appropriate
vtable reference based on the object's type, eliminating manual vtable management from
the frontend compiler.

---
name: elaboration
description: Elaborate Reussir source code.
license: MPL-2.0
---

You can use `reussir-elab` to track the type checking process of Reussir source 
code.

The command has two modes:

- `--mode semi`: Semi-elaborate the source code and print the elaborated AST.
  In the Semi IR, local expression types are inferred and checked but generic
  subsitutions are not attempted. Instead, only generic solutions are printed,
  which can be used for further instantiations.
- `--mode full`: Fully elaborate the source code and print the elaborated AST.
  In the Full IR, all types are checked and all generic subsitutions are
  attempted. Different from the Semi IR, the Full IR additionally insert
  modality for memory management. So, for Rc-managed objects, the values are
  now wrapped in Rc. Moreover, functions and records are now mangled without
  generic parameters.
  
For example, `cabal run reussir-elab -- --mode semi tests/integration/frontend/region.rr` will generate the following output:

```rust
;; Instantiated Generics
Generic @1 should be instantiated to:
Generic @0 should be instantiated to:
u64

;; Elaborated Records
struct [regional] Container<T@1>{
    cell: [field] Cell[regional]<T1>
}

struct [regional] Cell<T@0>{
    value: T0
}


;; Elaborated Functions
fn freeze_cell() -> Cell[rigid]<u64> {
    run_region{Cell<u64>(1.0 : u64) : Cell[flex]<u64>} : Cell[rigid]<u64>
}

fn trivial() -> u64 {
    run_region{1.0 : u64} : u64
}

regional fn regional_function(v0 (x): u64) -> Cell[flex]<u64> {
    Cell<u64>(v0) : Cell[flex]<u64>
}

fn freeze_cell_with_let_expr() -> Cell[rigid]<u64> {
    run_region{{
        let v0 (x) : Cell[flex]<u64> = regional_function[regional](42.0 : u64) : Cell[flex]<u64>;
        v0
    }} : Cell[rigid]<u64>
}
```

While `cabal run reussir-elab -- --mode full tests/integration/frontend/region.rr` will generate the following output:

```rust
;; Elaborated Full Records
struct _RIC4CellyE (aka Cell<u64>){
    value: u64
}


;; Elaborated Full Functions
regional fn _RC17regional_function(x: u64) -> Rc<_RIC4CellyE, Flex> {
    {
        rc_wrap(compound(v0) : _RIC4CellyE) : Rc<_RIC4CellyE, Flex>
    }
}

fn _RC25freeze_cell_with_let_expr() -> Rc<_RIC4CellyE, Rigid> {
    {
        run_region{{
            let v0 (x) : Rc<_RIC4CellyE, Flex> = _RC17regional_function[regional](42.0 : u64) : Rc<_RIC4CellyE, Flex>;
            v0
        }} : Rc<_RIC4CellyE, Rigid>
    }
}

fn _RC7trivial() -> u64 {
    {
        run_region{{
            1.0 : u64
        }} : u64
    }
}

fn _RC11freeze_cell() -> Rc<_RIC4CellyE, Rigid> {
    {
        run_region{{
            rc_wrap(compound(1.0 : u64) : _RIC4CellyE) : Rc<_RIC4CellyE, Flex>
        }} : Rc<_RIC4CellyE, Rigid>
    }
}
```

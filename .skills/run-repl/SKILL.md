---
name: run-repl
description: Run Reussir REPL to interpret Reussir source code.
license: MPL-2.0
---

You can use `reussir-repl` to run Reussir REPL.
code.

It has several CLI options:

```bash
reussir-repl - Read-Eval-Print Loop for Reussir

Usage: reussir-repl [-O|--opt-level ARG] [-l|--log-level ARG] [-i|--input ARG]

  Reussir REPL

Available options:
  -O,--opt-level ARG       Optimization level (none, default, aggressive, size,
                           tpde)
  -l,--log-level ARG       Log level (error, warning, info, debug, trace)
  -i,--input ARG           Input file for line-by-line execution
  -h,--help                Show this help text
```

The REPL can either read from a file or from stdin.

REPL provides convenient instructions to dump current state of the interpreter.

Assume we have the following input:

```bash
fn cos_squared(x: f64) -> f64 { let y = core::intrinsic::math::cos(x, 0); y * y }

fn sin_squared(x: f64) -> f64 { let y = core::intrinsic::math::sin(x, 0); y * y }

:dump context

fn poly_add<T : Num>(x : T) -> T { (cos_squared(x as f64) + sin_squared(x as f64)) as T }

poly_add(127)

poly_add(127.0)

:dump compiled
```

We can run it with:

```bash
cabal run reussir-repl -- -i tests/integration/repl/polymorphic.repl -lerror -Otpde
```

And we will get the following output:

```bash
Definition added.
Definition added.
=== Records ===

=== Functions ===
fn sin_squared(v0 (x): f64) -> f64 {
    {
        let v1 (y) : f64 = core::intrinsic::math::sin(v0, 0.0 : i32) : f64;
        (v1 * v1) : f64
    }
}
fn cos_squared(v0 (x): f64) -> f64 {
    {
        let v1 (y) : f64 = core::intrinsic::math::cos(v0, 0.0 : i32) : f64;
        (v1 * v1) : f64
    }
}
Definition added.
1 : i64
1.0 : f64
=== Compiled Functions ===
 - __repl_expr_1
 - __repl_expr_0
 - _RIC8poly_adddE
 - _RIC8poly_addxE
 - _RC11sin_squared
 - _RC11cos_squared
```


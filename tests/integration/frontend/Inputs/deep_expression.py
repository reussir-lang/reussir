#!/usr/bin/env python3
"""deep_expression.py OUT.rr: a program with deeply nested expressions.

`chain` adds 8000 calls (a left-nested `+` tree 8000 deep); `parens` wraps
a literal in 100000 parentheses. `deep_expression_check` returns
chain(1) + parens(): 8000 * 1 + 7999 * 8000 / 2 + 7 = 32004007.
"""
import sys

n, d = 8000, 100000
lines = [
    "fn f(x : u64, i : u64) -> u64 { x + i }",
    "fn chain(z : u64) -> u64 {",
    "    " + " + ".join(f"f(z, {i})" for i in range(n)),
    "}",
    "fn parens() -> u64 { " + "(" * d + "7" + ")" * d + " }",
    "pub fn check(z : u64) -> u64 { chain(z) + parens() }",
    'extern "C" trampoline "deep_expression_check" = check;',
]
open(sys.argv[1], "w").write("\n".join(lines) + "\n")

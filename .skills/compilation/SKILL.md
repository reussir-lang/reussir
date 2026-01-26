---
name: compilation
description: Compile Reussir source code.
license: MPL-2.0
---

You can use `reussir-compiler` to compile Reussir source code.
code.

It has several CLI options:

```bash
reussir-compiler - Reussir Compiler

Usage: reussir-compiler INPUT (-o|--output OUTPUT) [-O|--opt-level ARG] 
                        [-t|--target ARG] [-l|--log-level ARG] 
                        [-m|--module-name ARG]

  Compile a Reussir file

Available options:
  INPUT                    Input file
  -o,--output OUTPUT       Output file
  -O,--opt-level ARG       Optimization level (none, default, aggressive, size,
                           tpde)
  -t,--target ARG          Output target (llvm-ir, asm, object)
  -l,--log-level ARG       Log level (error, warning, info, debug, trace)
  -m,--module-name ARG     Module name
  -h,--help                Show this help text
```

As an example, to dump the llvm-ir, you can do something like:

```bash
cabal run reussir-compiler -- tests/integration/frontend/projection.rr -o /dev/fd/0 -tllvm-ir -Oaggressive
```

The first several lines will look like this:

```llvm
; ModuleID = 'main'
source_filename = "main"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

%_RIC3BoxIC5RcBoxIC5RcBoxIC5RcBoxIC3BoxIC3BoxmEEEEEE = type { ptr }

define weak_odr i32 @_RC10rc_project(ptr %0) local_unnamed_addr !dbg !3 {
  %2 = getelementptr i8, ptr %0, i64 8, !dbg !6
  %3 = load i32, ptr %2, align 4, !dbg !6
  ret i32 %3, !dbg !6
}
```

Using the `-ltrace` option, you can get more detailed information about the compilation process.

Although it can be quite verbose:
```
[2026-01-25 23:19:56.231] [reussir-compiler] [trace] reussir-compiler: Adding IR instruction: RefProject {refProjectVal = (Value 10,TypeRef (Ref {refInner = TypeExpr (Symbol "_RIC3BoxmE"), refAtomicity = NonAtomic, refCapability = Shared})), refProjectField = 0, refProjectRes = (Value 11,TypeRef (Ref {refInner = TypePrim (PrimInt PrimInt32), refAtomicity = NonAtomic, refCapability = Shared}))}
[2026-01-25 23:19:56.231] [reussir-compiler] [trace] reussir-compiler: Adding IR instruction: RefLoad {refLoadVal = (Value 11,TypeRef (Ref {refInner = TypePrim (PrimInt PrimInt32), refAtomicity = NonAtomic, refCapability = Shared})), refLoadRes = (Value 12,TypePrim (PrimInt PrimInt32))}
[2026-01-25 23:19:56.231] [reussir-compiler] [trace] reussir-compiler: Adding IR instruction: Return (Just (Value 12,TypePrim (PrimInt PrimInt32)))
[2026-01-25 23:19:56.231] [reussir-compiler] [trace] reussir-compiler: Lowering function Symbol "_RC10rc_project" with body
[2026-01-25 23:19:56.231] [reussir-compiler] [trace] reussir-compiler: Adding IR instruction: RcBorrow {rcBorrowVal = (Value 0,TypeRc (Rc {rcBoxInner = TypeExpr (Symbol "_RIC5RcBoxmE"), rcBoxAtomicity = NonAtomic, rcBoxCapability = Shared})), rcBorrowRes = (Value 1,TypeRef (Ref {refInner = TypeExpr (Symbol "_RIC5RcBoxmE"), refAtomicity = NonAtomic, refCapability = Shared}))}
[2026-01-25 23:19:56.231] [reussir-compiler] [trace] reussir-compiler: Adding IR instruction: RefProject {refProjectVal = (Value 1,TypeRef (Ref {refInner = TypeExpr (Symbol "_RIC5RcBoxmE"), refAtomicity = NonAtomic, refCapability = Shared})), refProjectField = 0, refProjectRes = (Value 2,TypeRef (Ref {refInner = TypePrim (PrimInt PrimInt32), refAtomicity = NonAtomic, refCapability = Shared}))}
[2026-01-25 23:19:56.231] [reussir-compiler] [trace] reussir-compiler: Adding IR instruction: RefLoad {refLoadVal = (Value 2,TypeRef (Ref {refInner = TypePrim (PrimInt PrimInt32), refAtomicity = NonAtomic, refCapability = Shared})), refLoadRes = (Value 3,TypePrim (PrimInt PrimInt32))}
[2026-01-25 23:19:56.231] [reussir-compiler] [trace] reussir-compiler: Adding IR instruction: Return (Just (Value 3,TypePrim (PrimInt PrimInt32)))
```

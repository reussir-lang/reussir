---
name: rene
description: Build Reussir packages with the rene package manager.
license: MPL-2.0
---

You can use `rene` (the Reussir package manager, built from `crates/rene`) to
build multi-module and multi-package Reussir projects. Build it with
`cmake --build build --target rene`; the binary lands at `build/bin/rene`.

A package is a directory with a `rene.ncl` manifest (Nickel syntax) and
sources under `src/` (`src/lib.rr` is the root module). A minimal manifest:

```nickel
{
  package = {
    name | String = "demo",
    version = "1.0.0",
  },
  dependencies = {
    std = { bundled = true, version = "^0.1" },
    textio = { path = "vendor/textio", version = "^0.5" },
  },
  targets = {
    demo = { kind = 'executable },
  },
}
```

Commands (`rene --help` and `rene build --help` for the full lists):

```bash
rene new mypkg                    # scaffold a package: commented rene.ncl,
                                  # src/lib.rr, git repo with reussir-build/
                                  # ignored (--vcs none to skip)
rene new mypkg --lib --staticlib  # target kinds (--bin/--lib/--staticlib,
                                  # combinable; --bin is the default)
rene new mypkg --target wasm32-wasip1  # cross target baked into the profiles
rene new mypkg -i                 # ask about each choice interactively
rene build                        # every declared target, dev profile
rene build --profile release      # a built-in (dev, release) or manifest profile
rene build --bin app --lib util   # only the named targets
rene build --target wasm32-wasip1 # cross-compile for a machine target
rene build -j 4                   # cap the compile-process pool
rene inspect --solved --graph     # dependency resolution + graph as JSON
rene inspect --commands           # the rrc commands the build would run
rene clean                        # delete the build directory
```

Both commands take `--manifest-path` and `--build-dir`; by default rene finds
the nearest `rene.ncl` at or above the current directory and builds into
`reussir-build/` next to it. Artifact paths are printed on stdout (progress
goes to stderr); a package declaring no targets stops after the runtime bake
and prints the library directories to pass to `rrc --polyffi-libdir` by hand.

`core` is bundled inside rene and injected into every package (opt out with
`package.no_core`). `std` is bundled too, but opt-in: `bundled = true`
instead of a `path`. Both are packed from `library/` when rene is built.

What every build has in common (the bundled libraries and the runtime bake)
lives in a per-user cache: `$XDG_CACHE_HOME/rene` (or `~/.cache/rene`) on
Linux, `~/Library/Caches/rene` on macOS, and `%LOCALAPPDATA%\rene` on
Windows. Set `RENE_CACHE_DIR` to use another directory; the lit suites point
it at `build/tests/integration/test/rene-cache`. Entries are built under a
file lock (a second rene waits and says so) and are safe to delete while no
rene runs. `rene clean` does not touch the cache.

`rene` shells out to `rrc`, `cargo`, and `rustc`; set `REUSSIR_RRC`,
`REUSSIR_CARGO`, or `REUSSIR_RUSTC` to pin specific binaries (otherwise
`rrc` is found through `PATH`).

Realistic end-to-end examples live under `tests/integration/rene/` — see the
README there; `calc/`, `inventory/`, and `stats/` are complete multi-package
programs with polyffi dependencies.

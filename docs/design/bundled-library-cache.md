# Bundled library compilation cache

`rene build` shares the bundled `core` package's compiled interface and static
archive across build directories through
[compcache](https://github.com/reussir-lang/compcache).

The store uses `Bypass` and `ManagedDirectory`: artifacts live only in leased
on-disk directories, with SQLite metadata and no memory cache or blob payloads.
The cache lives in the platform cache directory's `rene` subdirectory
(`$XDG_CACHE_HOME/rene` or `~/.cache/rene` on Linux, `~/Library/Caches/rene` on
macOS, and Local AppData's `rene` on Windows). `RENE_CACHE_DIR` overrides that
complete path; relative overrides resolve against the working directory.
The integration uses compcache's built-in pruning settings (4 GiB trigger,
seven-day retention, pruning on writes), without loading a configuration file.

Each versioned recipe includes bundled and materialized source digests, the
library manifest, compiler executable identity, Rust compiler version, target,
static-library codegen flags, optional report selection, and upstream artifact
digests. The flags include the project's optimization profile, sanitizers, CPU
features, reuse ABI and layout settings. Project output paths, runtime bake
locations and runtime source bundle identity are absent. Link-only settings do
not change a library bake. A local path dependency called `core` builds normally.

A cache miss compiles directly into compcache's managed directory. Declared
output files and the directory are synced before publication. No staging copy,
project copy, hard link or symbolic link is needed. compcache manages concurrent
production, recovery and eviction; its synchronous producer runs on a blocking
worker under rene's process admission bound.

Each dependency's selected interface, archive and optional report are recorded
as their actual cache paths. Compiler `--extern` arguments and linker inputs use
those paths directly. The cache handle owns reader leases until every dependent
compiler and final linker finishes. Producers also retain their upstream leases;
on error the scheduler drains admitted work before releasing the cache's leases.
A static-library cache entry may be pruned after the build returns, without
affecting linked outputs. The next build reacquires a lease and rebuilds a missing entry as needed.
Source bundles remain local to each project. The Rust runtime is globally
shared as described below.

Cache failures are reported with `RENE_CACHE_DIR` in the diagnostic; compiler
errors retain their normal diagnostics. Failed compilation never publishes a
partial library. CLI tests use isolated cache directories.

## Globally shared Rust runtime

`reussir-rt` is built once per Rust toolchain, target, runtime source bundle,
allocator backend and sanitizer/Rust flags. Its immutable Cargo output stays in
compcache's managed directory; every project with that configuration consumes
the same paths. Project optimization profiles do not repeat this fixed-release
Rust runtime build. The core/std library recipes independently retain the
project's optimization, reuse ABI, sanitizer and layout settings.

One cache handle owns the store and active runtime/library leases. The
`runtime.json` record inside a runtime entry uses relative artifact paths;
project metadata only records the selected paths. Cargo's target and build
directories are pinned inside the cache entry. A project-selected rustup toolchain
is resolved before changing working directory, so the global cache does not
change which Rust compiler builds its runtime.

The lit suite shares `RENE_CACHE_DIR` across fixtures, so same-target/runtime
settings reuse one Cargo bake. The native cache tests also use that shared root;
isolated fake-toolchain CLI tests verify cold publication, failure recovery and
pruning.

Runtime entries use the `runtimes/` store below the same global cache root.
They are retained until explicit cleanup, because an explicitly dynamic-linked
program may need its runtime after the build has ended. Automatic size/age
pruning applies to the core/std static-library store. Both stores bypass the
memory cache and use managed on-disk directories.

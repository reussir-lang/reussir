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

Each v0 recipe includes bundled and materialized source digests, the
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
those paths directly. Runtime and library artifact handles own
`Arc<RuntimeLease>` guards. Planned commands clone their input guards into
compiler invocations, including blocking cache producers, so input files remain
protected through subprocess completion.
The graph retains library handles between stages; runtime handles retain the
runtime. On error the scheduler drains admitted work. Lease ownership needs no
shared registry or mutex.
A static-library cache entry may be pruned after the build returns, without
affecting linked outputs. The next build reacquires a lease and rebuilds a missing entry as needed.
Source bundles remain local to each project. The Rust runtime is globally
shared as described below.

Cache failures use typed `thiserror` variants and identify `RENE_CACHE_DIR` in
their diagnostics. Producer errors retain their typed source chains, including
compiler, I/O and decoding failures; display formatting happens at the CLI or
terminal UI boundary. Failed compilation never publishes a partial library.
CLI tests use isolated cache directories.

## Globally shared Rust runtime

`reussir-rt` is built once per Rust toolchain, target, runtime source bundle,
allocator backend and sanitizer/Rust flags. Its immutable Cargo output stays in
compcache's managed directory; every project with that configuration consumes
the same paths. Project optimization profiles do not repeat this fixed-release
Rust runtime build. The core/std library recipes independently retain the
project's optimization, reuse ABI, sanitizer and layout settings.

The cache handle owns only the store. The `runtime.json` record inside a runtime
entry uses relative artifact paths; project metadata only records the selected
paths, never live lease ownership. Cargo's target and build directories are
pinned inside the cache entry. A project-selected rustup toolchain
is resolved before changing working directory, so the global cache does not
change which Rust compiler builds its runtime.

The lit suite shares `RENE_CACHE_DIR` across fixtures, so same-target/runtime
settings reuse one Cargo bake. The native cache tests also use that shared root;
isolated fake-toolchain CLI tests verify cold publication, failure recovery and
pruning.

Runtime and core/std entries share one compcache store, including its metadata,
object directory and automatic size/age pruning policy. The `rene-rust-runtime-v0`
and `rene-bundled-library-v0` namespaces distinguish their recipes. The store bypasses the
memory cache and uses managed on-disk directories.

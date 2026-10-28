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

## Bundled std

The std source ships inside rene and uses the same disk cache after its core
dependency is ready. It remains opt-in: declare
`dependencies.std = { version = "^0.1" }` in `rene.ncl` to select the shipped
version. Omitting the dependency
neither loads nor builds std. The normal version solver checks the constraint;
an explicit `path` selects a local std checkout and bypasses the bundled cache.
Transitive path dependencies can request bundled std in the same way. Packages
outside the shipped bundles continue to require a local path.

Only each library's `rene.ncl` and `src/` are embedded. Build directories and
other checkout files never enter the bundle or change its cache identity.

## Persistent PJRT kernel compilation

The PJRT-enabled runtime uses compcache directly, with application name
`reussir`. A synchronous miss compiles and serializes the executable into SQLite;
a later process deserializes those bytes with its own PJRT client. A resident
hit returns a client-owned `Arc<Executable>`. Executables retain their clients,
so eviction or cache shutdown does not invalidate callers' handles. Serialization
or cache-write failures retain a successfully compiled payload and report the
cache failure. Actual compiler errors remain typed failures and are not cached.

The executable's v0 recipe includes the verified bytecode digest, compile
options, donation mapping/donors/aliases, plugin digest, platform version,
serialized topology and `XLA_FLAGS`. A checksum or deserialization failure causes
one eviction/retry. Refinement keys separately include argument shapes and
constant values. compcache coalesces requests with the same key while allowing
different kernels to compile concurrently. Native destruction happens outside
cache locks. The cache has no private executor or background I/O workers.

Configuration follows compcache: `reussir/config.ncl` under the platform config
root, and `reussir` under its cache root. `REUSSIR_CONFIG_DIR` and
`REUSSIR_CACHE_DIR` override the complete directories. Memory weights, pruning,
SQLite and retry settings use compcache's schema and defaults. The former
`REUSSIR_PJRT_CACHE_CONFIG` TOML settings and foyer storage files are no longer
used.

Rene embeds the Cargo-pinned compcache sources beside `reussir-rt`, rewrites the
runtime dependency to that local path, and adjusts its seeded lock entry.
Standalone Cargo resolution prunes the workspace's superset lock while retaining
its version/source pins; it does not need the original compcache checkout.

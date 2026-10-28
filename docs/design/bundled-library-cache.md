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

Each versioned recipe includes the bundled source digest, rene version, full
compiler executable digest, actual source digests, evaluated manifest, Rust toolchain, runtime bundle digest, target,
planned commands, upstream artifact digests, and relevant Rust environment
flags. Only known build-directory and bundled-source arguments are normalized;
user flags and external library paths remain part of the recipe. A dependency
called `core` that comes from a user path is compiled normally.

A cache miss compiles the interface and archive using the existing plan. Only
successful, complete outputs (including the token-reuse report when requested)
are streamed through compcache's `ArtifactWriter`. compcache serializes producers
across processes and manages publication, recovery and eviction. Its synchronous
work runs on a blocking worker under rene's process admission limit.

A hit copies the artifacts into the project's ordinary dependency directory
while holding a reader lease. Copies are independent files, so rebuilding or
editing one project cannot modify cached artifacts, and cache pruning cannot
race their restoration. The existing source scanner and freshness records still
track each project's build. Source bundles and the Rust runtime bake continue
to live in each project build directory.

Cache failures are reported with `RENE_CACHE_DIR` in the diagnostic; compiler
errors retain their normal diagnostics. Failed compilation never publishes a
partial library. Tests use isolated cache directories.

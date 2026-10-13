# Polymorphic-FFI cache storage

`reussir-ffi-cache` is an independent Rust library for compiler-generated bitcode.
It uses Foyer 0.22.6, following the PJRT artifact cache's filesystem block device,
Psync I/O, byte-weighted memory budget, exclusive directory lock, and close-time
flush. It has no dependency on PJRT, LLVM, or the language runtime.

```rust,no_run
use reussir_ffi_cache::{Cache, Config};
let cache = Cache::open(Config::new("/trusted/application/cache"))?;
cache.insert(b"complete compiler input digest", b"bitcode")?;
let bitcode = cache.get(b"complete compiler input digest")?;
cache.close()?;
# Ok::<(), reussir_ffi_cache::Error>(())
```

Opening is explicit; this crate does not read environment variables or select a
cache directory. Defaults are 64 MiB key/value memory capacity, 1 GiB disk capacity,
and 16 MiB blocks. Configurations require page-aligned memory >= 8 KiB,
page-aligned blocks >= 16 KiB, and at least four whole disk blocks. An entry must
fit both the memory budget and a block with a page reserved for record overhead.
Memory accounting excludes runtime metadata, I/O buffers, request copies, and
caller-owned result copies. Requests use a bounded queue. Foyer owns eviction,
recovery and record checksums. Checksums detect damage, not malicious changes.

A dedicated owner thread drives a Tokio runtime. The safe Rust API is synchronous
and thread-safe, including calls from within an existing Tokio runtime. `close`
and `Drop` flush writes, stop/join workers, and then release the directory lock.
Explicit close reports failures; Drop ignores them. A crash may lose pending
writes; cache admission is not a durable-storage guarantee. Disk eviction can
turn an earlier insertion into a miss.

The directory must be trusted and dedicated to this cache: `.lock`, `layout`, and
`data/` belong to the library. A changed storage format, block size, or disk
capacity resets `data/` under the lock, including surplus files after shrinking.
Only one process/handle may open a directory at a time; contention returns an
error immediately. Compilers should treat open/get/insert failures as cache misses
and continue compiling. The caller owns content keys and their invalidation;
this layer cannot identify missing compiler dependencies.

`include/Reussir/RustCompiler/Cache.h` declares the panic-contained C ABI. It uses
opaque handles, explicit byte lengths, owned result buffers, and distinguishes
hits, misses, and errors. A result remains valid after the cache closes. Calls
contain unwinding Rust panics, but cannot recover from allocation aborts or invalid
C pointers. Unix paths preserve native bytes; Windows paths are UTF-8. The Psync
backend implements both Unix positional I/O and Windows seek-read/write.

The crate emits an `rlib` for Rust consumers and a `staticlib` for C++ tools.
Consumers must link the Rust static library's platform-native dependencies; Rust
programs should depend on the `rlib` instead of embedding a second Rust standard
library. Build-system and compiler-cache policy wiring live outside this crate.

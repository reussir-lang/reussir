# reussir-polyffi-cache

A standalone C ABI for compcache's database-only opaque byte cache. This crate
uses `Store<Bypass, NoExtraData>`: it retains no resident payloads and creates no
managed artifact directory. Values are stored verbatim as SQLite BLOBs.

The API is in `include/reussir_polyffi_cache.h`. Open an explicit cache directory,
then use `get`, `get_or_produce`, and `evict` with opaque 32-byte keys. The optional
`key_from_parts` helper delegates directly to compcache's binary recipe API; the
caller chooses its namespace and recipe. An optional Nickel configuration file
uses compcache's existing parser. No environment or application defaults are
selected by this binding.

A generic producer runs synchronously only on a miss, under compcache's per-key
lease. Successful production returns a blob even when persistence or subsequent
maintenance fails; an optional error handle then describes the warning. Producer
failures are not retried. A producer can return transient bytes with `persist=0`.

Cache, blob, and error handles have explicit destroy functions. Blob bytes remain
valid until their blob handle is destroyed, including after cache eviction or
cache destruction. The API does not transfer caller buffers to Rust. Error
messages are borrowed UTF-8 spans and include the underlying typed error chain.

The binding defines no compiler recipe, checksums/framing, validation, retry or
fallback policy. It does not invoke rustc or LLVM and is not wired into compiler
consumers. Adoption is a separate change. Standard compcache database, WAL/SHM,
and lock files remain necessary; there are no external payload files.

Build and test explicitly with `cargo build -p reussir-polyffi-cache` and
`cargo test -p reussir-polyffi-cache`. The crate produces an rlib and a staticlib;
caller-specific build-system linkage belongs to adoption.

The standalone `tests/c_api.cpp` fixture exercises the public header and staticlib
without any compiler consumer. For a native Unix build, run the following from
the workspace root with `CARGO_BUILD_TARGET` unset. The native libraries come from
the selected rustc and its actual dependency closure, rather than a portable list
of guessed linker flags. The checks remain active with `NDEBUG` defined.

```sh
set -eu
cache_abi_dir=$(mktemp -d)
trap 'rm -rf "$cache_abi_dir"' EXIT
cargo rustc --locked -p reussir-polyffi-cache --lib \
  -- --print=native-static-libs 2>"$cache_abi_dir/rustc.log" || {
  cat "$cache_abi_dir/rustc.log"
  exit 1
}
native_libs=$(sed -n 's/^note: native-static-libs: //p' "$cache_abi_dir/rustc.log")
test -n "$native_libs"
# Intentional word splitting: rustc reports a sequence of linker arguments.
c++ -std=c++17 -DNDEBUG -Wall -Wextra -Werror \
  -I crates/reussir-polyffi-cache/include \
  crates/reussir-polyffi-cache/tests/c_api.cpp \
  "${CARGO_TARGET_DIR:-target}/debug/libreussir_polyffi_cache.a" \
  $native_libs -o "$cache_abi_dir/c-api"
"$cache_abi_dir/c-api" "$cache_abi_dir/cache"
```

# reussir-polyffi-cache

A C binding for `Store<Bypass, NoExtraData>`. Open a store, call
`get_or_produce` with a caller-owned 32-byte key, then close it. A hit skips the
producer; a miss runs it under compcache's key lock. Bytes go verbatim into
SQLite, with no resident byte cache or managed artifact files.

The public header is `include/reussir_polyffi_cache.h`. Result and diagnostic
callbacks borrow bytes synchronously, so the caller copies any bytes it needs.
The producer returns false on failure, or sets `persist=false` when its output
should be returned without publication. A write failure reports a diagnostic
while preserving successful output. Cache failures and producer failures have
distinct return codes, allowing the caller to choose its fallback.

There is no compiler integration. Keys, validation, and compilation remain the
caller's responsibility. Build and test with `cargo test -p reussir-polyffi-cache`.
The standalone `tests/c_api.cpp` exercises the public ABI:

```sh
set -eu
cache_abi_dir=$(mktemp -d)
trap 'rm -rf "$cache_abi_dir"' EXIT
cargo rustc --locked -p reussir-polyffi-cache --lib \
  -- --print=native-static-libs 2>"$cache_abi_dir/rustc.log"
native_libs=$(sed -n 's/^note: native-static-libs: //p' "$cache_abi_dir/rustc.log")
test -n "$native_libs"
c++ -std=c++17 -DNDEBUG -Wall -Wextra -Werror \
  -I crates/reussir-polyffi-cache/include crates/reussir-polyffi-cache/tests/c_api.cpp \
  "${CARGO_TARGET_DIR:-target}/debug/libreussir_polyffi_cache.a" \
  $native_libs -o "$cache_abi_dir/c-api"
"$cache_abi_dir/c-api" "$cache_abi_dir/cache"
```

//===- Cache.h - Bounded storage for polymorphic-FFI bitcode -----*- C -*-===//
//
// Part of the Reussir project, licensed under Apache-2.0 OR MIT.
//
//===----------------------------------------------------------------------===//
#ifndef REUSSIR_RUSTCOMPILER_CACHE_H
#define REUSSIR_RUSTCOMPILER_CACHE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ReussirFfiCache ReussirFfiCache;
typedef struct ReussirFfiCacheBuffer ReussirFfiCacheBuffer;

// Explicitly opens a trusted, dedicated directory, exclusively locked until
// close. Path bytes are native bytes on Unix, UTF-8 on Windows; no terminator is
// needed. Zero capacities select defaults: 16 MiB I/O buffers, 1 GiB disk, 16 MiB
// blocks. Returns NULL on errors, invalid config, or lock contention.
// No retained memory value cache. I/O buffers, pending writes, metadata and
// caller-owned results still consume memory.
ReussirFfiCache *reussir_ffi_cache_open(const uint8_t *directory,
                                     size_t directory_len, size_t io_buffer_bytes,
                                     size_t disk_bytes, size_t block_bytes);

// Keys/invalidation belong to the caller. Returns 1 on hit, 0 on miss, -1 on
// error. Sets *output to NULL before lookup, or transfers an owned buffer on hit.
// A buffer remains valid after the cache closes and must be freed exactly once.
int reussir_ffi_cache_get(const ReussirFfiCache *cache, const uint8_t *key,
                         size_t key_len, ReussirFfiCacheBuffer **output);

// Copies both inputs, returning 0 on admission, -1 on error/oversized entry.
// Admission is not durability: entries may be evicted and close flushes writes.
int reussir_ffi_cache_insert(const ReussirFfiCache *cache, const uint8_t *key,
                            size_t key_len, const uint8_t *value,
                            size_t value_len);

const uint8_t *
reussir_ffi_cache_buffer_data(const ReussirFfiCacheBuffer *buffer);
size_t reussir_ffi_cache_buffer_size(const ReussirFfiCacheBuffer *buffer);
void reussir_ffi_cache_buffer_free(ReussirFfiCacheBuffer *buffer);

// Flushes and joins workers before releasing the lock. Consumes the handle even
// on failure. Returns 0 on success, -1 on error. NULL is a successful no-op.
int reussir_ffi_cache_close(ReussirFfiCache *cache);

// All functions contain Rust unwinding panics (not allocation aborts). Null
// byte pointers are permitted only for zero length. Cache get/insert calls may
// overlap; close must have exclusive access. Reads must not overlap buffer_free.
// Null cache get/insert returns -1. Null buffer data/size returns NULL/0; freeing
// NULL is a no-op. Every non-null pointer must be live and valid for its length.

#ifdef __cplusplus
}
#endif
#endif // REUSSIR_RUSTCOMPILER_CACHE_H

#ifndef REUSSIR_POLYFFI_CACHE_H
#define REUSSIR_POLYFFI_CACHE_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

typedef struct ReussirPolyffiCache ReussirPolyffiCache;
typedef struct ReussirPolyffiCacheBlob ReussirPolyffiCacheBlob;
typedef struct ReussirPolyffiCacheError ReussirPolyffiCacheError;
typedef struct {
  uint8_t bytes[32];
} ReussirPolyffiCacheKey;
typedef struct {
  const uint8_t *data;
  size_t size;
  uint8_t persist;
} ReussirPolyffiCacheProduced;
/* Called synchronously on a miss, under compcache's per-key lease. Return
 * zero for success or an application-defined nonzero failure code. The byte
 * span must remain valid until get_or_produce returns. persist is zero or one.
 * Callbacks must not unwind or recursively request the same cache key. */
typedef uint32_t (*ReussirPolyffiCacheProducer)(
    void *user_data, ReussirPolyffiCacheProduced *result);

enum ReussirPolyffiCacheStatus {
  REUSSIR_POLYFFI_CACHE_OK = 0,
  REUSSIR_POLYFFI_CACHE_MISS = 1,
  REUSSIR_POLYFFI_CACHE_INVALID_ARGUMENT = 2,
  REUSSIR_POLYFFI_CACHE_ERROR = 3,
  REUSSIR_POLYFFI_CACHE_PRODUCER_ERROR = 4,
  REUSSIR_POLYFFI_CACHE_PANIC = 5
};
/* All byte spans are borrowed for the call; null is allowed only when empty.
 * Handles are owned and must be destroyed once. Destroying null is harmless.
 * Error output pointers are optional. Other output pointers are required.
 * Handle outputs are initialized to null on failure/miss; a key output is
 * written only on success. Do not overwrite a live output handle. Cache
 * operations may run concurrently; destruction requires them to finish. There
 * is no memory admission, managed storage, or implicit environment policy. The
 * binding adds no framing, validation, or retry policy; compcache retains its
 * configured I/O/database retries. Only opaque bytes enter SQLite. */
uint32_t reussir_polyffi_cache_key_from_parts(const uint8_t *domain,
                                              size_t domain_size,
                                              const uint8_t *recipe,
                                              size_t recipe_size,
                                              ReussirPolyffiCacheKey *result,
                                              ReussirPolyffiCacheError **error);
/* Directory is a native byte path on Unix and UTF-8 on Windows. An empty
 * config path uses compcache defaults; otherwise it names a Nickel config.
 * Configuration cannot enable memory admission or managed artifact storage. */
uint32_t reussir_polyffi_cache_open(const uint8_t *directory,
                                    size_t directory_size,
                                    const uint8_t *config_path,
                                    size_t config_path_size,
                                    ReussirPolyffiCache **result,
                                    ReussirPolyffiCacheError **error);
void reussir_polyffi_cache_destroy(ReussirPolyffiCache *cache);
uint32_t reussir_polyffi_cache_get(const ReussirPolyffiCache *cache,
                                   const ReussirPolyffiCacheKey *key,
                                   ReussirPolyffiCacheBlob **result,
                                   ReussirPolyffiCacheError **error);
/* OK can return both a usable blob and an error handle describing a cache
 * persistence/maintenance warning. A producer failure returns no blob and is
 * never retried. A hit never invokes producer. */
uint32_t reussir_polyffi_cache_get_or_produce(
    const ReussirPolyffiCache *cache, const ReussirPolyffiCacheKey *key,
    ReussirPolyffiCacheProducer producer, void *user_data,
    ReussirPolyffiCacheBlob **result, ReussirPolyffiCacheError **error);
uint32_t reussir_polyffi_cache_evict(const ReussirPolyffiCache *cache,
                                     const ReussirPolyffiCacheKey *key,
                                     ReussirPolyffiCacheError **error);
/* The returned span is borrowed until blob_destroy. A blob survives cache
 * eviction and cache destruction. size may be null. */
const uint8_t *
reussir_polyffi_cache_blob_data(const ReussirPolyffiCacheBlob *blob,
                                size_t *size);
void reussir_polyffi_cache_blob_destroy(ReussirPolyffiCacheBlob *blob);
/* Message is UTF-8, borrowed until error_destroy, and not NUL terminated.
 * A null error has status OK and an empty message. size may be null. */
uint32_t
reussir_polyffi_cache_error_kind(const ReussirPolyffiCacheError *error);
const uint8_t *
reussir_polyffi_cache_error_message(const ReussirPolyffiCacheError *error,
                                    size_t *size);
void reussir_polyffi_cache_error_destroy(ReussirPolyffiCacheError *error);
#ifdef __cplusplus
}
#endif
#endif

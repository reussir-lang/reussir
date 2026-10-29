#ifndef REUSSIR_POLYFFI_CACHE_H
#define REUSSIR_POLYFFI_CACHE_H
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif

typedef struct ReussirPolyffiCache ReussirPolyffiCache;
typedef struct {
  const uint8_t *data;
  size_t size;
} ReussirPolyffiCacheProduced;
typedef bool (*ReussirPolyffiCacheProducer)(void *,
                                            ReussirPolyffiCacheProduced *);
typedef void (*ReussirPolyffiCacheBytes)(const uint8_t *, size_t, void *);

/* Open with compcache's rrc application defaults.
 * A null return reports failure through diagnostic. */
ReussirPolyffiCache *
reussir_polyffi_cache_open(ReussirPolyffiCacheBytes diagnostic, void *context);

/* Cache and key must be valid; callbacks are synchronous and must not unwind
 * or recursively request the same key. Producer returns false on failure;
 * otherwise its bytes are cached. Producer bytes must remain live until this
 * call returns. Result/diagnostic spans are borrowed only during their callback.
 * Returns 0 on success (possibly with a cache write warning), 1 on cache failure,
 * or 2 on producer failure. */
uint32_t reussir_polyffi_cache_get_or_produce(
    const ReussirPolyffiCache *cache, const uint8_t key[32],
    ReussirPolyffiCacheProducer producer, ReussirPolyffiCacheBytes result,
    ReussirPolyffiCacheBytes diagnostic, void *context);

/* Close once, after requests finish. Null is harmless. */
void reussir_polyffi_cache_close(ReussirPolyffiCache *cache);
#ifdef __cplusplus
}
#endif
#endif

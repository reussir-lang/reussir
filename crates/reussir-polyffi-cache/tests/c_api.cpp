#include "reussir_polyffi_cache.h"
#include <cstdlib>
#include <cstring>
#include <type_traits>

static_assert(sizeof(ReussirPolyffiCacheKey) == 32);
static_assert(std::is_standard_layout_v<ReussirPolyffiCacheProduced>);
static void check(bool condition) {
  if (!condition)
    std::abort();
}
static const uint8_t payload[] = {0, 255, 1, 0, 42};
static uint32_t produce(void *context, ReussirPolyffiCacheProduced *result) {
  ++*static_cast<unsigned *>(context);
  *result = {payload, sizeof(payload), 1};
  return 0;
}
int main(int argc, char **argv) {
  check(argc == 2);
  ReussirPolyffiCache *cache = nullptr;
  ReussirPolyffiCacheError *error = nullptr;
  check(reussir_polyffi_cache_open(nullptr, 1, nullptr, 0, &cache, &error) ==
        REUSSIR_POLYFFI_CACHE_INVALID_ARGUMENT);
  check(!cache && error);
  check(reussir_polyffi_cache_error_kind(error) ==
        REUSSIR_POLYFFI_CACHE_INVALID_ARGUMENT);
  size_t message_size = 0;
  const auto *message =
      reussir_polyffi_cache_error_message(error, &message_size);
  check(message && message_size != 0);
  reussir_polyffi_cache_error_destroy(error);
  error = nullptr;
  auto status = reussir_polyffi_cache_open(
      reinterpret_cast<const uint8_t *>(argv[1]), std::strlen(argv[1]), nullptr,
      0, &cache, &error);
  check(status == REUSSIR_POLYFFI_CACHE_OK && cache && !error);
  ReussirPolyffiCacheKey key{};
  check(reussir_polyffi_cache_key_from_parts(
            reinterpret_cast<const uint8_t *>("c-api"), 5,
            reinterpret_cast<const uint8_t *>("opaque"), 6, &key,
            &error) == REUSSIR_POLYFFI_CACHE_OK);
  ReussirPolyffiCacheBlob *blob = nullptr;
  check(reussir_polyffi_cache_get(cache, &key, &blob, &error) ==
        REUSSIR_POLYFFI_CACHE_MISS);
  unsigned calls = 0;
  check(reussir_polyffi_cache_get_or_produce(cache, &key, produce, &calls,
                                             &blob, &error) ==
        REUSSIR_POLYFFI_CACHE_OK);
  check(calls == 1 && blob && !error);
  ReussirPolyffiCacheBlob *hit = nullptr;
  check(reussir_polyffi_cache_get_or_produce(cache, &key, produce, &calls, &hit,
                                             &error) ==
        REUSSIR_POLYFFI_CACHE_OK);
  check(calls == 1 && hit && !error);
  reussir_polyffi_cache_blob_destroy(hit);
  check(reussir_polyffi_cache_evict(cache, &key, &error) ==
        REUSSIR_POLYFFI_CACHE_OK);
  reussir_polyffi_cache_destroy(cache);
  size_t size = 0;
  const auto *data = reussir_polyffi_cache_blob_data(blob, &size);
  check(size == sizeof(payload) && std::memcmp(data, payload, size) == 0);
  reussir_polyffi_cache_blob_destroy(blob);
  check(reussir_polyffi_cache_error_kind(nullptr) == REUSSIR_POLYFFI_CACHE_OK);
  reussir_polyffi_cache_error_destroy(nullptr);
}

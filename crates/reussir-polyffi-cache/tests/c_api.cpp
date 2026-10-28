#include "reussir_polyffi_cache.h"
#include <cstdlib>
#include <vector>

static void check(bool condition) {
  if (!condition)
    std::abort();
}
struct Request {
  unsigned calls = 0;
  std::vector<uint8_t> bytes;
};
static bool produce(void *context, ReussirPolyffiCacheProduced *result) {
  ++static_cast<Request *>(context)->calls;
  static const uint8_t bytes[] = {0, 255, 42};
  *result = {bytes, sizeof(bytes), true};
  return true;
}
static void receive(const uint8_t *data, size_t size, void *context) {
  static_cast<Request *>(context)->bytes.assign(data, data + size);
}
static void diagnostic(const uint8_t *, size_t, void *) { std::abort(); }
int main() {
  Request request;
  auto *cache = reussir_polyffi_cache_open(diagnostic, &request);
  check(cache);
  const uint8_t key[32] = {};
  for (unsigned i = 0; i != 2; ++i)
    check(reussir_polyffi_cache_get_or_produce(cache, key, produce, receive,
                                               diagnostic, &request) == 0);
  reussir_polyffi_cache_close(cache);
  check(request.calls == 1);
  check(request.bytes == std::vector<uint8_t>({0, 255, 42}));
}

#include <assert.h>
#include <stdint.h>
#include <string.h>

extern uint64_t simd_clamp(int32_t *base, uint64_t len, int32_t limit);
extern uint64_t simd_count(const uint8_t *base, uint64_t len, uint8_t needle);
extern uint64_t simd_find(const uint8_t *base, uint64_t len, uint8_t needle);
extern int64_t simd_sum(const int32_t *base, uint64_t len);
extern uint64_t simd_count_scalable(const uint8_t *base, uint64_t len,
                                    uint8_t needle);
extern uint64_t simd_find_scalable(const uint8_t *base, uint64_t len,
                                   uint8_t needle);

int main(void) {
  // A guard byte after each buffer checks that the tail is never written.
  uint8_t text[41];
  for (int i = 0; i < 40; ++i)
    text[i] = (uint8_t)("abcdefghij"[i % 10]);
  text[40] = 'c';
  for (uint64_t len = 0; len <= 40; ++len) {
    uint64_t expected = 0, first = len;
    for (uint64_t i = 0; i < len; ++i) {
      if (text[i] == 'c') {
        expected += 1;
        if (first == len)
          first = i;
      }
    }
    assert(simd_count(text, len, 'c') == expected);
    assert(simd_find(text, len, 'c') == first);
    assert(simd_find(text, len, 'z') == len);
    assert(simd_count_scalable(text, len, 'c') == expected);
    assert(simd_find_scalable(text, len, 'c') == first);
    assert(simd_find_scalable(text, len, 'z') == len);
  }

  int32_t values[20];
  for (uint64_t len = 0; len <= 19; ++len) {
    int64_t total = 0;
    for (int i = 0; i < 20; ++i)
      values[i] = i * 3 - 7;
    for (uint64_t i = 0; i < len; ++i)
      total += values[i];
    assert(simd_sum(values, len) == total);

    uint64_t above = 0;
    for (uint64_t i = 0; i < len; ++i)
      above += values[i] > 20;
    assert(simd_clamp(values, len, 20) == above);
    for (uint64_t i = 0; i < len; ++i) {
      int32_t original = (int32_t)i * 3 - 7;
      assert(values[i] == (original > 20 ? 20 : original));
    }
    // The element after the range is untouched.
    assert(values[len] == (int32_t)len * 3 - 7);
  }
  return 0;
}

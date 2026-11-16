#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

extern uint64_t simd_keep(const int32_t *from, int32_t *to, uint64_t len);
extern uint64_t simd_lookup(const int32_t *table, int32_t *data, uint64_t len);
extern void simd_bump(int64_t *counters, const uint32_t *keys);
extern void simd_spread(const int32_t *from, const bool *mask, int32_t *to);

int main(void) {
  int32_t source[21], kept[22];
  for (int i = 0; i < 21; ++i)
    source[i] = (i % 3 == 0) ? -i : i;
  for (uint64_t len = 0; len <= 21; ++len) {
    for (int i = 0; i < 22; ++i)
      kept[i] = 1000;
    uint64_t expected = 0;
    int32_t reference[21];
    for (uint64_t i = 0; i < len; ++i)
      if (source[i] > 0)
        reference[expected++] = source[i];
    assert(simd_keep(source, kept, len) == expected);
    for (uint64_t i = 0; i < expected; ++i)
      assert(kept[i] == reference[i]);
    // Nothing is written after the kept elements.
    assert(kept[expected] == 1000);
  }

  int32_t table[256];
  for (int i = 0; i < 256; ++i)
    table[i] = i * i - 5;
  int32_t data[20];
  for (uint64_t len = 0; len <= 19; ++len) {
    for (int i = 0; i < 20; ++i)
      data[i] = (i * 37 + 200) % 256;
    assert(simd_lookup(table, data, len) == len);
    for (uint64_t i = 0; i < len; ++i)
      assert(data[i] == table[((int)i * 37 + 200) % 256]);
    // The key after the range is not translated.
    assert(data[len] == ((int)len * 37 + 200) % 256);
  }

  int64_t counters[16] = {0};
  const uint32_t keys[8] = {15, 0, 7, 3, 9, 12, 1, 4};
  simd_bump(counters, keys);
  simd_bump(counters, keys);
  for (int i = 0; i < 16; ++i) {
    bool touched = false;
    for (int k = 0; k < 8; ++k)
      touched |= keys[k] == (uint32_t)i;
    assert(counters[i] == (touched ? 2 : 0));
  }

  const int32_t packed[3] = {10, 20, 30};
  const bool mask[8] = {false, true, false, false, true, true, false, false};
  int32_t wide[8];
  simd_spread(packed, mask, wide);
  const int32_t spread[8] = {-1, 10, -1, -1, 20, 30, -1, -1};
  for (int i = 0; i < 8; ++i)
    assert(wide[i] == spread[i]);
  return 0;
}

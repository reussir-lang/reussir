#include <assert.h>
#include <stdint.h>

extern void shape_matmul(const float *a, const float *b, float *out);
extern void shape_mirror(const uint8_t *from, uint8_t *to);
extern void shape_words(const uint8_t *from, uint32_t *to);

int main(void) {
  float a[16], b[16], out[16];
  for (int i = 0; i < 16; ++i) {
    a[i] = (float)(i + 1);
    b[i] = (float)((i * 7) % 5 - 2);
  }
  shape_matmul(a, b, out);
  for (int i = 0; i < 4; ++i)
    for (int j = 0; j < 4; ++j) {
      float expected = 0;
      for (int k = 0; k < 4; ++k)
        expected += a[i * 4 + k] * b[k * 4 + j];
      assert(out[i * 4 + j] == expected);
    }

  const uint8_t bytes[8] = {10, 11, 12, 13, 14, 15, 16, 17};
  uint8_t mirrored[8];
  shape_mirror(bytes, mirrored);
  // Reversed is 17..10; swapping each pair gives 16, 17, 14, 15, ...
  const uint8_t expected_bytes[8] = {16, 17, 14, 15, 12, 13, 10, 11};
  for (int i = 0; i < 8; ++i)
    assert(mirrored[i] == expected_bytes[i]);

  uint8_t raw[16];
  for (int i = 0; i < 16; ++i)
    raw[i] = (uint8_t)(i == 4 ? 100 : i == 8 ? 200 : 0);
  uint32_t words[4];
  shape_words(raw, words);
  // The middle words are 100 + 1 and 200 + 2; the second row is replaced.
  assert(words[0] == 101 && words[1] == 202 && words[2] == 9 && words[3] == 9);
  return 0;
}

#include <assert.h>
#include <stdint.h>
#include <stdbool.h>
extern uint64_t scalar(uint64_t), dimensions(uint64_t), empty(uint64_t);
extern int64_t scalar_fill(int64_t), sharded(uint64_t, uint64_t), isolation(int64_t);
extern bool bool_copy(bool);
extern void float_copy(float *, const float *);
extern int32_t replicated(int32_t);
int main(void) {
  assert(bool_copy(true) && !bool_copy(false));
  float input = 1.25f, output = 0;
  float_copy(&output, &input);
  assert(output == input);
  assert(replicated(17) == 51);
  assert(scalar(UINT64_MAX) == UINT64_MAX);
  assert(scalar_fill(INT64_MIN) == INT64_MIN);
  for (uint64_t n = 0; n < 6; ++n) {
    assert(dimensions(n) == 2 * n + 5);
    assert(empty(n) == n);
    for (uint64_t m = 0; m < 6; ++m) {
      int64_t expected = 0;
      for (uint64_t i = 0; i < n; ++i)
        for (uint64_t j = 0; j < m; ++j)
          expected += 2 * (10 * i + j) + 7;
      assert(sharded(n, m) == expected);
    }
  }
  assert(isolation(42) == 42 + 99 + 88);
}

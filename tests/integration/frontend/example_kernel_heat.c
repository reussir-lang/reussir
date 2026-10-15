#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
/* Float-return trampolines use an output pointer and packed argument storage. */
struct SampleArgs { int64_t steps, index; };
extern void heat_sample(float *result, const struct SampleArgs *args);
extern void heat_energy(float *result, const int64_t *steps);
static float sample(int64_t steps, int64_t index) {
  const struct SampleArgs args = {steps, index};
  float result;
  heat_sample(&result, &args);
  return result;
}
static float energy(int64_t steps) {
  float result;
  heat_energy(&result, &steps);
  return result;
}
static void reference(int steps, float values[8]) {
  for (int i = 0; i < 8; ++i) values[i] = i == 4 ? 1.0f : 0.0f;
  for (int step = 0; step < steps; ++step) {
    float next[8];
    for (int i = 0; i < 8; ++i)
      next[i] = 0.5f * values[i] + 0.25f * (values[(i + 7) % 8] + values[(i + 1) % 8]);
    for (int i = 0; i < 8; ++i) values[i] = next[i];
  }
}
int main(void) {
  const int steps[] = {0, 1, 2, 10, 50};
  for (unsigned k = 0; k < sizeof steps / sizeof steps[0]; ++k) {
    float expected[8];
    reference(steps[k], expected);
    printf("heat %2d:", steps[k]);
    for (int i = 0; i < 8; ++i) {
      float actual = sample(steps[k], i);
      assert(isfinite(actual) && actual >= 0.0f);
      assert(fabsf(actual - expected[i]) < 0.00001f);
      printf(" %.4f", actual);
    }
    assert(fabsf(energy(steps[k]) - 1.0f) < 0.00002f);
    putchar('\n');
  }
  return 0;
}

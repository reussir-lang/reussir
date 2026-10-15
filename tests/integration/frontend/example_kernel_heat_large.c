#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>
struct Metrics { float maximum_error, energy; };
extern void heat_large(struct Metrics *result, const int64_t *steps);
int main(void) {
  const int64_t steps[] = {0, 1, 32, 128};
  for (unsigned i = 0; i < sizeof steps / sizeof steps[0]; ++i) {
    struct Metrics result;
    struct timespec begin, end;
    timespec_get(&begin, TIME_UTC);
    heat_large(&result, &steps[i]);
    timespec_get(&end, TIME_UTC);
    assert(isfinite(result.maximum_error) && result.maximum_error < 0.00002f);
    assert(isfinite(result.energy) && fabsf(result.energy - 1.0f) < 0.0002f);
    double seconds = (double)(end.tv_sec - begin.tv_sec) + 1e-9 * (double)(end.tv_nsec - begin.tv_nsec);
    printf("heat 1024 cells, %3lld steps: max error %.8g, energy %.8f, %.3f s\n",
           (long long)steps[i], result.maximum_error, result.energy, seconds);
  }
  return 0;
}

// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
static unsigned checked;
void check(int64_t test, int64_t index, float actual) {
  static const float scale[] = {1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1};
  static const float bias[] = {1, 1, 0, 0, 0, 5, 0, 5, 1, 0, 0, 1, 0};
  if (test < 0 || test >= 13 || index < 0 || index >= 4)
    abort();
  float expected = scale[test] * (index + 1) + bias[test];
  if (actual != expected) {
    fprintf(stderr, "case %ld element %ld: %g != %g\n", (long)test, (long)index,
            actual, expected);
    abort();
  }
  ++checked;
}
extern void examples(void);
int main(void) {
  examples();
  if (checked != 52)
    abort();
  return 0;
}

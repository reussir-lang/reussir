#include <assert.h>
#include <stddef.h>
#include <stdio.h>

static size_t checks;
void check(size_t index, float value) {
  const float expected[] = {12, 24, 36, 48, 11, 22, 33, 44, -9, -18, -27, -36};
  assert(index < sizeof(expected) / sizeof(*expected));
  assert(value == expected[index]);
  ++checks;
}
extern void examples(void);
int main(void) {
  examples();
  examples();
  assert(checks == 40);
  puts("SAXPY, repeated inputs, multiple outputs, and empty calls passed");
  return 0;
}

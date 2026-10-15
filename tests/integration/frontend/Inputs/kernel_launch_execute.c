#include <assert.h>
#include <stddef.h>
#include <stdio.h>
static size_t checks;
void check(size_t index, float value) {
  assert(index < 4);
  assert(value == (float)(index + 1));
  ++checks;
}
extern void examples(void);
int main(void) {
  examples();
  examples();
  assert(checks == 24);
  puts("source host launches, configuration, reuse, and async kernels passed");
}

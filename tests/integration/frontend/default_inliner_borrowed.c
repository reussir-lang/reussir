#include <stdint.h>
#include <stdio.h>

extern int64_t demo(int64_t n);

int main(void) {
  for (int64_t n = 0; n < 16; ++n) {
    int64_t actual = demo(n);
    if (actual != 2 * (n + 2)) {
      fprintf(stderr, "demo(%lld): expected %lld, got %lld\n", (long long)n,
              (long long)(2 * (n + 2)), (long long)actual);
      return 1;
    }
  }
  return 0;
}

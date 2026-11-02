#include <inttypes.h>
#include <stdio.h>

extern int64_t demo(int64_t n);

int main(void) {
  for (int64_t n = 0; n < 16; ++n) {
    int64_t actual = demo(n);
    if (actual != 2 * (n + 2)) {
      fprintf(stderr,
              "demo(%" PRId64 "): expected %" PRId64 ", got %" PRId64 "\n", n,
              2 * (n + 2), actual);
      return 1;
    }
  }
  return 0;
}

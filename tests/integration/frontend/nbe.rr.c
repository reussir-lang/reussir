#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern int32_t nbe_test_ffi(void);

int main(void) {
  int32_t n = nbe_test_ffi();
  if (n != 1000) {
    fprintf(stderr, "FAIL: expected 1000, got %d\n", n);
    abort();
  }
  return 0;
}

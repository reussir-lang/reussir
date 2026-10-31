#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern int64_t gdb_inlined_ffi(int64_t n);

int main(void) {
  /* Twice the sum of 1..4. */
  int64_t r = gdb_inlined_ffi(4);
  if (r != 20) {
    fprintf(stderr, "FAIL: got %lld\n", (long long)r);
    abort();
  }
  return 0;
}

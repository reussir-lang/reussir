#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern int64_t membership_ffi(int64_t n, int64_t queries);

int main(void) {
  /* Four queries over a million cells; the two that miss walk the whole
     list, so only a preserved tail call survives them. */
  int64_t hits = membership_ffi(1000000, 4);
  if (hits != 2) {
    fprintf(stderr, "FAIL: expected 2, got %lld\n", (long long)hits);
    abort();
  }
  return 0;
}

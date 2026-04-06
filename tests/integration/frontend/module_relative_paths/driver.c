#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

uint64_t entry_ffi(uint64_t);

int main(void) {
  uint64_t result = entry_ffi(7);
  if (result != 39) {
    fprintf(stderr, "unexpected result: %llu\n", (unsigned long long)result);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

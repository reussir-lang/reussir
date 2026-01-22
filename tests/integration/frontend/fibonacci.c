#include <stdint.h>
#include <stdlib.h>

#ifndef FIB_SYMBOL
#define FIB_SYMBOL _RC13fibonacci_u64
#endif

extern uint64_t FIB_SYMBOL(uint64_t n);

int main() {
  if (FIB_SYMBOL(10) != 55)
    abort();
  if (FIB_SYMBOL(42) != 267914296)
    abort();
  return 0;
}

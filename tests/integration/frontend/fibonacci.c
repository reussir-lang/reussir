#include <stdint.h>
#include <stdlib.h>

extern uint64_t fibonacci_ffi(uint64_t n);

int main() {
  if (fibonacci_ffi(10) != 55)
    abort();
  if (fibonacci_ffi(42) != 267914296)
    abort();
  return 0;
}

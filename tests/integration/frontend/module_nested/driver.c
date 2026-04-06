#include <stdint.h>
#include <stdlib.h>

extern uint64_t nested_entry_ffi(uint64_t n);

int main() {
  // double(5) + offset() = 10 + 42 = 52
  if (nested_entry_ffi(5) != 52)
    abort();
  // double(0) + offset() = 0 + 42 = 42
  if (nested_entry_ffi(0) != 42)
    abort();
  // double(100) + offset() = 200 + 42 = 242
  if (nested_entry_ffi(100) != 242)
    abort();
  return 0;
}

#include <stdint.h>

uint64_t count(uint64_t n);
uint64_t nested(uint64_t n);

int main(void) {
  // `count` runs its body three times in all, and `nested` two times.
  return count(3) + count(0) + nested(2) == 5 ? 0 : 1;
}

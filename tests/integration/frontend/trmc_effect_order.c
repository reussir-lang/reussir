#include <assert.h>
#include <stdint.h>

extern int64_t last_store(int64_t n);
extern int64_t reads(int64_t n);

int main(void) {
  assert(last_store(5) == 5);
  assert(reads(3) == 21);
  return 0;
}

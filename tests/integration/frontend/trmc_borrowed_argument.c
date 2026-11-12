#include <assert.h>
#include <stdint.h>

extern int64_t borrowed_sum(int64_t n);

int main(void) {
  assert(borrowed_sum(3) == 1207);
  return 0;
}

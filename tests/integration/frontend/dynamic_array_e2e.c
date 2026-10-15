#include <assert.h>
#include <stdint.h>
extern int64_t managed(uint64_t);
int main(void) {
  for (uint64_t n = 0; n < 20; ++n)
    assert(managed(n) == (n ? (int64_t)(14*n + n*(n-1)/2 + 11) : 0));
  return 0;
}

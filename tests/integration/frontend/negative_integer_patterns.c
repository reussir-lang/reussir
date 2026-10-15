#include <assert.h>
#include <stdint.h>
extern int64_t score(int64_t);
int main(void) {
  assert(score(INT64_MIN) == 1);
  assert(score(-128) == 2);
  assert(score(-1) == 3);
  assert(score(0) == 0);
  assert(score(INT64_MAX) == INT64_MAX);
  return 0;
}

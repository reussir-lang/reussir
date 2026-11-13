#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

extern void *ptr_identity(void *p);
extern const void *ptr_pick(bool first, const void *p, const void *q);
extern void *ptr_hold(void *p);
extern int64_t ptr_bump(void *p);
extern bool ptr_null_is_null(void);

int main(void) {
  int64_t a = 41;
  int64_t b = 0;
  assert(ptr_identity(&a) == &a);
  assert(ptr_identity(0) == 0);
  assert(ptr_pick(true, &a, &b) == &a);
  assert(ptr_pick(false, &a, &b) == &b);
  assert(ptr_hold(&b) == &b);
  assert(ptr_bump(&a) == 42);
  assert(a == 42);
  assert(ptr_null_is_null());
  return 0;
}

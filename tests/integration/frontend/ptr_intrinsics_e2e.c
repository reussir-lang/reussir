#include <assert.h>
#include <stdint.h>

extern int64_t ptr_sum(const void *base, int64_t count);
extern int64_t ptr_fill(void *base, int64_t count, int32_t value);
extern uint64_t ptr_stride(void);
extern uint64_t ptr_untag(void *p);
extern int32_t ptr_rebuild(void *p);
extern int32_t ptr_retag(const void *p);
extern int32_t ptr_rewrite(void *p, int32_t value);
extern int32_t ptr_relaunder(void *p, int32_t value);
extern uint64_t ptr_flagged(void *p, uint64_t value);
extern int64_t ptr_chase(const void *slot);

int main(void) {
  int32_t values[4] = {1, 2, 3, 4};
  assert(ptr_sum(values, 4) == 10);
  assert(ptr_sum(values, 0) == 0);
  assert(ptr_fill(values, 3, 7) == 3 * (int64_t)sizeof(int32_t));
  assert(values[0] == 7 && values[2] == 7 && values[3] == 4);
  assert(ptr_stride() == sizeof(int32_t));
  assert(ptr_untag((char *)values + 3) == ((uintptr_t)values + 3) % 8);
  assert(ptr_rebuild(values) == 7);
  assert(ptr_retag(values + 2) == 4);
  assert(ptr_rewrite(values + 1, 9) == 9 && values[1] == 9);
  assert(ptr_relaunder(values + 3, 5) == 9 && values[3] == 9);
  char bytes[9];
  assert(ptr_flagged(bytes + 1, 0x0102030405060708ull) == 0x0102030405060708ull);
  double target = 25.0;
  const double *slot = &target;
  assert(ptr_chase(&slot) == 25);
  return 0;
}

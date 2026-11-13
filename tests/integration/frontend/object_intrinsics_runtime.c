#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

extern int64_t object_counts(int64_t n);
extern bool object_cell_counts(void);
extern bool object_recursion_counts(void);

int main(void) {
  assert(object_counts(42) == 42);
  assert(object_cell_counts());
  assert(object_recursion_counts());
  return 0;
}

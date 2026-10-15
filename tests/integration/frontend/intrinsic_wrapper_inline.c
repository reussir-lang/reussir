#include <assert.h>
#include <stdint.h>
extern uint64_t wrapper_order(uint64_t);
extern uint64_t wrapper_managed(void);
int main(void) {
  assert(wrapper_order(0) == 33);
  assert(wrapper_order(7) == 54);
  for (int i = 0; i < 100; ++i) assert(wrapper_managed() == 24);
  return 0;
}

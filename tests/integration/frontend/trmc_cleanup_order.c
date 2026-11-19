#include <assert.h>
#include <stdint.h>

const void *live_slot;
extern uint64_t held(uint64_t n);
extern uint64_t captured(uint64_t n);

int main(void) {
  assert(held(3) == 3);
  assert(captured(3) == 3);
  return 0;
}

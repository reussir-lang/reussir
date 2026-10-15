#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

static size_t shard_checks, scalar_checks;
void check_shard(size_t index, int32_t value) {
  static const int32_t expected[] = {3, 7, 11, 19, 23};
  assert(index < 5 && value == expected[index]);
  ++shard_checks;
}
void check_unsigned(uintptr_t address, uint64_t expected) {
  assert(*(const uint64_t *)address == expected);
  ++scalar_checks;
}
extern void shape_examples(void);
int main(void) {
  shape_examples();
  shape_examples();
  assert(shard_checks == 20 && scalar_checks == 4);
  puts("source launches preserve uneven shards, scalar bits, nested tuples, and live inputs");
  return 0;
}

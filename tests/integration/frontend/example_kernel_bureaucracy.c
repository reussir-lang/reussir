#include <assert.h>
#include <stdint.h>
#include <stdio.h>
extern int64_t bureaucracy(int64_t rounds, int64_t stamp);
int main(void) {
  const int64_t rounds[] = {0, 1, 2, 10, 20};
  const int64_t expected[] = {0, 1, 1, 55, 6765};
  for (int repeat = 0; repeat < 2; ++repeat)
    for (unsigned i = 0; i < sizeof rounds / sizeof rounds[0]; ++i) {
      assert(bureaucracy(rounds[i], 42) == expected[i]);
      assert(bureaucracy(rounds[i], -7) == expected[i]);
    }
  puts("Fibonacci approved; all parcels survived the bureaucracy.");
  return 0;
}

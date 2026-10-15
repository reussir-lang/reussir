#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#define DECLARE(name) extern uint64_t name(uint64_t)
DECLARE(counter); DECLARE(swap); DECLARE(effects); DECLARE(zero_state);
DECLARE(managed); DECLARE(managed_many); DECLARE(nested); DECLARE(outer_scope);
DECLARE(captured_loop); DECLARE(regional_loop); DECLARE(replace_unused); DECLARE(condition_capture);
static void check(const char *name, uint64_t actual, uint64_t expected) {
  if (actual != expected) { fprintf(stderr, "%s: got %llu, expected %llu\n", name, (unsigned long long)actual, (unsigned long long)expected); abort(); }
}
#define CHECK(name, expected) check(#name, name(n), (expected))
int main(void) {
  for (uint64_t n = 0; n < 25; ++n) {
    CHECK(regional_loop, n); CHECK(replace_unused, n); CHECK(condition_capture, n);
    CHECK(counter, n); CHECK(swap, n * 100 + (n % 2 ? 21 : 12));
    CHECK(effects, n * 10000 + (n + 1) * 100 + n + 3);
    CHECK(zero_state, n); CHECK(managed, n + 7); CHECK(managed_many, 2 * n + 30);
    CHECK(nested, n); CHECK(outer_scope, n + 7); CHECK(captured_loop, (n < 5 ? 5 : n) + 5);
  }
}

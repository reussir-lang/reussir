#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define DECLARE(name) extern uint64_t name##_ffi(uint64_t)
DECLARE(tuple_all_units);
DECLARE(tuple_frozen);
DECLARE(tuple_atomic);
DECLARE(tuple_unit_match);
DECLARE(tuple_unit_bind);
DECLARE(tuple_unit_capture);
DECLARE(tuple_unit_partial);
DECLARE(tuple_unit_order);
DECLARE(tuple_native_unit);
DECLARE(tuple_basic);
DECLARE(tuple_once);
DECLARE(tuple_units);
DECLARE(tuple_duplicate);
DECLARE(tuple_partial);
DECLARE(tuple_records);
DECLARE(tuple_cell);
DECLARE(tuple_refcell);
DECLARE(tuple_arrow);
DECLARE(tuple_closure);
DECLARE(tuple_partial_apply);
DECLARE(tuple_array);
DECLARE(tuple_fold);
DECLARE(tuple_match);
DECLARE(tuple_cartesian);

struct Pair {
  uint64_t first, second;
};
struct PackedPair {
  struct Pair value;
};
extern void tuple_native_ffi(struct Pair *, const struct PackedPair *);

static void check(const char *name, uint64_t actual, uint64_t expected) {
  if (actual != expected) {
    fprintf(stderr, "%s: got %llu, expected %llu\n", name,
            (unsigned long long)actual, (unsigned long long)expected);
    abort();
  }
}

#define CHECK(name, expected) check(#name, name##_ffi(n), (expected))
int main(void) {
  for (uint64_t n = 0; n < 32; ++n) {
    CHECK(tuple_all_units, n + 2);
    CHECK(tuple_frozen, 2 * n);
    CHECK(tuple_atomic, 3 * n + 1);
    CHECK(tuple_unit_match, n + 32);
    CHECK(tuple_unit_bind, n + 27);
    CHECK(tuple_unit_capture, 2 * n + 3);
    CHECK(tuple_unit_partial, 4 * n + 3);
    CHECK(tuple_unit_order, 1101 * n + 3002);
    CHECK(tuple_native_unit, n + 1);
    CHECK(tuple_basic, 3 * n + 3);
    CHECK(tuple_once, 111 * n + 12);
    CHECK(tuple_units, 2 * n + 1);
    CHECK(tuple_duplicate, 3 * n);
    CHECK(tuple_partial, 2 * n + 11);
    CHECK(tuple_records, 4 * n + 5);
    CHECK(tuple_cell, 4 * n + 22);
    CHECK(tuple_refcell, n + 3);
    CHECK(tuple_arrow, 4 * n + 3);
    CHECK(tuple_closure, 5 * n + 5);
    CHECK(tuple_partial_apply, 4 * n + 7);
    CHECK(tuple_array, 2 * n + 5);
    CHECK(tuple_fold, 4 * n + 4);
    CHECK(tuple_match, n == 0 ? 10 : n + (n > 5 ? 20 : 30));
    CHECK(tuple_cartesian, n == 0 ? 1 : n < 2 ? 3 : 4);
    struct PackedPair args = {{n, n + 10}};
    struct Pair result = {0, 0};
    tuple_native_ffi(&result, &args);
    check("native first", result.first, n + 10);
    check("native second", result.second, n + 1);
  }
  return 0;
}

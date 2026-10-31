#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern int64_t run_unique(int64_t n, int64_t k);
extern int64_t run_shared(int64_t n, int64_t k);

void *__real___reussir_allocate_small(size_t size);

static int64_t allocations;

void *__wrap___reussir_allocate_small(size_t size) {
  ++allocations;
  return __real___reussir_allocate_small(size);
}

static int64_t count(int64_t (*run)(int64_t, int64_t), int64_t want) {
  int64_t before = allocations;
  int64_t got = run(2000, 100);
  if (got != want) {
    fprintf(stderr, "FAIL: expected %lld, got %lld\n", (long long)want,
            (long long)got);
    abort();
  }
  return allocations - before;
}

int main(void) {
  /* 100 rebuilds of a 2000-cell list in each context. */
  int64_t unique = count(run_unique, 101);
  int64_t shared = count(run_shared, 200);
  if (unique > 2000 + 100) {
    fprintf(stderr, "FAIL: the unique loop allocated %lld cells: reuse lost\n",
            (long long)unique);
    abort();
  }
  if (shared < 100 * 2000) {
    fprintf(stderr, "FAIL: the shared loop allocated only %lld cells\n",
            (long long)shared);
    abort();
  }
  return 0;
}

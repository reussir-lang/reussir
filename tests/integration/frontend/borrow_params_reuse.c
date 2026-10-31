#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern int64_t run_unique(int64_t n, int64_t k);
extern int64_t run_shared(int64_t n, int64_t k);
extern int64_t dag_run(int64_t d, int64_t k);

static void expect(const char *what, int64_t got, int64_t want) {
  if (got != want) {
    fprintf(stderr, "FAIL: %s: expected %lld, got %lld\n", what,
            (long long)want, (long long)got);
    abort();
  }
}

int main(void) {
  /* 100 rebuilds of a 2000-cell list [1..2000] in each context. */
  expect("unique", run_unique(2000, 100), 101);
  expect("shared", run_shared(2000, 100), 200);
  /* Four folds over 2^16 paths of a 17-node DAG. */
  expect("dag", dag_run(16, 4), 4 * 65536);
  return 0;
}

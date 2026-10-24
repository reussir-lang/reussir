//===----------------------------------------------------------------------===//
//
// Part of the Reussir Project, dual licensed under the Apache License v2.0 or
// the MIT License.
// See https://github.com/reussir-lang/reussir/blob/main/LICENSE for license
// information.
// SPDX-License-Identifier: Apache-2.0 OR MIT
//
//===----------------------------------------------------------------------===//
// Driver for arc_bigint_parallel.rr: THREADS workers share one `Arc<Nat>`.
// `worker_ffi` takes its two scalar arguments directly; `dup_ffi` returns its
// struct through a leading pointer and takes its argument through a pointer to
// a packed argument struct, as in cells_sync_parallel_e2e.c.

#include <omp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct dup_arg {
  void *s;
};
struct pair {
  void *keep;
  void *extra;
};

extern void *make_shared_ffi(void);
extern void dup_ffi(struct pair *out, struct dup_arg *in);
extern int64_t worker_ffi(void *shared, int64_t steps);
extern int64_t added_ffi(void *);

enum { THREADS = 4, STEPS = 2000 };

int main(void) {
  void *shared = make_shared_ffi();
  void *handles[THREADS];
  for (int i = 0; i < THREADS; ++i) {
    struct dup_arg in = {shared};
    struct pair out;
    dup_ffi(&out, &in);
    handles[i] = out.extra;
    shared = out.keep;
  }

  int64_t checks = 0;
  omp_set_num_threads(THREADS);
#pragma omp parallel reduction(+ : checks)
  {
    checks += worker_ffi(handles[omp_get_thread_num()], STEPS);
  }

  int64_t got = added_ffi(shared);
  int64_t want = (int64_t)THREADS * STEPS;
  if (got != want || checks != want) {
    fprintf(stderr, "added %lld, checks %lld, want %lld\n", (long long)got,
            (long long)checks, (long long)want);
    return EXIT_FAILURE;
  }
  printf("shared Arc<Nat>: %lld\n", (long long)got);
  return EXIT_SUCCESS;
}

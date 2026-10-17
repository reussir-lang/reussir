// Driver for rwlock-cell-tsan.rr: shares one RwLock<i64> between four
// threads, each running ITERS write+read rounds, then prints the total.
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>

#define THREADS 4
#define ITERS 2000

struct pair {
  void *a;
  void *b;
};

extern void *make_ffi(void);
extern void dup_ffi(struct pair *out, void **in);
extern int64_t work_ffi(void *lock, int64_t iters);
extern int64_t total_ffi(void *lock);

static void *run(void *lock) {
  // work_ffi consumes the thread's reference.
  work_ffi(lock, ITERS);
  return NULL;
}

int main(void) {
  void *lock = make_ffi();
  pthread_t threads[THREADS];
  for (int i = 0; i < THREADS; ++i) {
    struct pair split;
    dup_ffi(&split, &lock);
    lock = split.a;
    pthread_create(&threads[i], NULL, run, split.b);
  }
  for (int i = 0; i < THREADS; ++i)
    pthread_join(threads[i], NULL);
  // total_ffi consumes the last reference.
  printf("total = %lld\n", (long long)total_ffi(lock));
  return 0;
}

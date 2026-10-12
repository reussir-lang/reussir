#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <sys/resource.h>
#endif
#ifdef __cplusplus
extern "C" {
#endif
typedef struct ReussirDtorExecutor ReussirDtorExecutor;
typedef struct ReussirDtorTask ReussirDtorTask;
typedef struct ReussirDtorStep ReussirDtorStep;
typedef void (*ReussirDtorStart)(void *buffer, void *object,
                                 ReussirDtorStep *result);
typedef void (*ReussirDtorResume)(void *buffer, bool cancel,
                                  ReussirDtorStep *result);
typedef struct ReussirDtorRoutine {
  ReussirDtorStart start;
  size_t size;
  size_t align;
} ReussirDtorRoutine;
struct ReussirDtorTask {
  const ReussirDtorRoutine *routine;
  void *object;
};
struct ReussirDtorStep {
  ReussirDtorResume resume;
  const ReussirDtorTask *child;
};
size_t __reussir_dtor_executor_size(void);
size_t __reussir_dtor_executor_align(void);
ReussirDtorExecutor *__reussir_create_dtor_executor(void *storage);
void __reussir_destroy_dtor_executor(ReussirDtorExecutor *context);
void *__reussir_dtor_conti_alloc(ReussirDtorExecutor *context, size_t size,
                                 size_t align);
void __reussir_dtor_conti_free(ReussirDtorExecutor *context);
void __reussir_run_dtor(ReussirDtorExecutor *context, void *object,
                        const ReussirDtorRoutine *routine);

extern const ReussirDtorRoutine routine_a;
static uintptr_t expected;
void observe(uint64_t n) { assert(n == expected++); }
void *unexpected_alloc(size_t size) {
  (void)size;
  abort();
}
void unexpected_free(void *ptr) {
  (void)ptr;
  abort();
}
void fill_payload(void *ptr) {
  assert((uintptr_t)ptr % 256 == 0);
  memset(ptr, 0xa5, 1024);
}
void check_payload(void *ptr) {
  assert((uintptr_t)ptr % 256 == 0);
  for (size_t i = 0; i < 1024; ++i)
    assert(((unsigned char *)ptr)[i] == 0xa5);
}
#ifdef __cplusplus
}
#endif
int main(void) {
#ifndef _WIN32
  struct rlimit limit;
  assert(!getrlimit(RLIMIT_STACK, &limit));
  if (limit.rlim_cur > 256 * 1024)
    limit.rlim_cur = 256 * 1024;
  assert(!setrlimit(RLIMIT_STACK, &limit));
#endif
  size_t size = __reussir_dtor_executor_size();
  size_t align = __reussir_dtor_executor_align();
  void *allocation = malloc(size + align - 1);
  assert(allocation);
  void *storage = (void *)(((uintptr_t)allocation + align - 1) & ~(align - 1));
  ReussirDtorExecutor *executor = __reussir_create_dtor_executor(storage);
  unsigned char *first =
      (unsigned char *)__reussir_dtor_conti_alloc(executor, 8193, 4096);
  assert((uintptr_t)first % 4096 == 0);
  memset(first, 0x5a, 8193);
  for (int i = 0; i < 1000; ++i)
    __reussir_dtor_conti_alloc(executor, 17 + i, 64);
  for (int i = 0; i < 8193; ++i)
    assert(first[i] == 0x5a);
  for (int i = 0; i < 1001; ++i)
    __reussir_dtor_conti_free(executor);
  for (int i = 0; i < 2; ++i) {
    expected = 0;
    __reussir_run_dtor(executor, (void *)(uintptr_t)100000, &routine_a);
    assert(expected == 100001);
  }
  __reussir_destroy_dtor_executor(executor);
  executor = __reussir_create_dtor_executor(storage);
  expected = 0;
  __reussir_run_dtor(executor, NULL, &routine_a);
  assert(expected == 1);
  __reussir_destroy_dtor_executor(executor);
  free(allocation);
}

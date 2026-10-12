#include "Reussir/Runtime/DtorExecutor.h"
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#ifdef __cplusplus
extern "C" {
#endif
extern ReussirDtorStep start_a(void *, void *);
static uintptr_t expected;
void observe(uint64_t n) { assert(n == expected++); }
#ifdef __cplusplus
}
#endif
int main(void) {
  struct rlimit limit;
  assert(!getrlimit(RLIMIT_STACK, &limit));
  if (limit.rlim_cur > 256 * 1024)
    limit.rlim_cur = 256 * 1024;
  assert(!setrlimit(RLIMIT_STACK, &limit));
  size_t size = __reussir_dtor_executor_size();
  size_t align = __reussir_dtor_executor_align();
  void *allocation = malloc(size + align - 1);
  assert(allocation);
  void *storage = (void *)(((uintptr_t)allocation + align - 1) & ~(align - 1));
  ReussirDtorExecutor *executor = __reussir_create_dtor_executor(storage);
  unsigned char *first = (unsigned char *)__reussir_dtor_conti_alloc(executor);
  assert((uintptr_t)first % REUSSIR_DTOR_CONTI_ALIGN == 0);
  memset(first, 0x5a, REUSSIR_DTOR_CONTI_SIZE);
  for (int i = 0; i < 1000; ++i)
    __reussir_dtor_conti_alloc(executor);
  for (int i = 0; i < REUSSIR_DTOR_CONTI_SIZE; ++i)
    assert(first[i] == 0x5a);
  for (int i = 0; i < 1001; ++i)
    __reussir_dtor_conti_free(executor);
  for (int i = 0; i < 2; ++i) {
    expected = 0;
    __reussir_run_dtor(executor, (void *)(uintptr_t)100000, start_a);
    assert(expected == 100001);
  }
  __reussir_destroy_dtor_executor(executor);
  executor = __reussir_create_dtor_executor(storage);
  expected = 0;
  __reussir_run_dtor(executor, NULL, start_a);
  assert(expected == 1);
  __reussir_destroy_dtor_executor(executor);
  free(allocation);
}

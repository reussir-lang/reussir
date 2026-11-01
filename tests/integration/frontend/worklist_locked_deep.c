#include <stdint.h>
#include <sys/resource.h>

extern void *locked_prepend(void *);
extern int64_t locked_discard(void *);

int main(void) {
  struct rlimit limit;
  if (getrlimit(RLIMIT_STACK, &limit))
    return 1;
  limit.rlim_cur = 256 * 1024;
  if (setrlimit(RLIMIT_STACK, &limit))
    return 2;
  void *node = 0;
  for (uint64_t i = 0; i < 100000; ++i)
    node = locked_prepend(node);
  return locked_discard(node) == 42 ? 0 : 3;
}

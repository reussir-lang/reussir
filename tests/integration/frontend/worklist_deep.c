#include <stdint.h>
#include <sys/resource.h>

extern void *deep_empty(void);
extern void *deep_wrap(void *);
extern int64_t deep_discard(void *);
extern void *deep_base(void);
extern void *deep_wrap_closure_a(void *);
extern void *deep_wrap_closure_b(void *);
extern int64_t deep_discard_closure(void *);

int main(void) {
  struct rlimit limit;
  if (getrlimit(RLIMIT_STACK, &limit))
    return 1;
  limit.rlim_cur = 256 * 1024;
  if (setrlimit(RLIMIT_STACK, &limit))
    return 2;
  void *node = deep_empty();
  void *closure = deep_base();
  for (uint64_t i = 0; i < 100000; ++i) {
    node = deep_wrap(node);
    closure =
        i % 2 ? deep_wrap_closure_a(closure) : deep_wrap_closure_b(closure);
  }
  if (deep_discard(node) != 42)
    return 3;
  return deep_discard_closure(closure) == 42 ? 0 : 4;
}

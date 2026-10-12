#include <stdint.h>
#include <sys/resource.h>

extern void *tail_nil(void);
extern void *tail_cons(uint64_t, void *);
extern uint64_t tail_shared(void *);
extern void *tail_node(uint64_t, void *);
extern uint64_t tail_node_head(void *);
extern void *tail_a_nil(void);
extern void *tail_a_cons(uint64_t, void *);
extern uint64_t tail_a_head(void *);

int main(void) {
  struct rlimit limit;
  if (getrlimit(RLIMIT_STACK, &limit))
    return 1;
  limit.rlim_cur = 256 * 1024;
  if (setrlimit(RLIMIT_STACK, &limit))
    return 2;
  void *list = tail_nil();
  void *node = 0;
  void *mutual = tail_a_nil();
  const uint64_t count = 200000;
  for (uint64_t i = 1; i <= count; ++i) {
    list = tail_cons(i, list);
    node = tail_node(i, node);
    mutual = tail_a_cons(i, mutual);
  }
  if (tail_shared(list) != 2 * count)
    return 3;
  if (tail_node_head(node) != count)
    return 4;
  return tail_a_head(mutual) == count ? 0 : 5;
}

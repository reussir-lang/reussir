#include <stdbool.h>
#include <stdint.h>
#include <sys/resource.h>

extern void *work_tree_empty(void);
extern void *work_tree_branch(uint64_t, void *, bool);
extern uint64_t work_tree_shared(void *);
extern void *work_rose_empty(void);
extern void *work_rose(uint64_t, void *);
extern uint64_t work_rose_head(void *);
extern void *work_snoc(uint64_t, void *);
extern uint64_t work_snoc_head(void *);
extern void *work_locked(uint64_t, void *);
extern uint64_t work_locked_head(void *);
extern void *work_flat(uint64_t, void *);
extern uint64_t work_flat_head(void *);
extern void *work_rw(uint64_t, void *);
extern uint64_t work_rw_head(void *);

int main(void) {
  struct rlimit limit;
  if (getrlimit(RLIMIT_STACK, &limit))
    return 1;
  limit.rlim_cur = 256 * 1024;
  if (setrlimit(RLIMIT_STACK, &limit))
    return 2;
  const uint64_t count = 100000;
  for (int left = 0; left < 2; ++left) {
    void *tree = work_tree_empty();
    for (uint64_t i = 1; i <= count; ++i)
      tree = work_tree_branch(i, tree, left);
    if (work_tree_shared(tree) != 2 * count)
      return 3;
  }
  void *rose = work_rose_empty();
  void *snoc = 0, *locked = 0, *flat = 0, *rw = 0;
  for (uint64_t i = 1; i <= count; ++i) {
    rose = work_rose(i, rose);
    snoc = work_snoc(i, snoc);
    locked = work_locked(i, locked);
    flat = work_flat(i, flat);
    rw = work_rw(i, rw);
  }
  if (work_rose_head(rose) != count)
    return 4;
  if (work_snoc_head(snoc) != count)
    return 5;
  if (work_locked_head(locked) != count)
    return 6;
  if (work_flat_head(flat) != count)
    return 7;
  if (work_rw_head(rw) != count)
    return 8;
  return 0;
}

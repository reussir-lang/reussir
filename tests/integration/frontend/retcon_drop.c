#include <assert.h>
#include <stdint.h>
#ifndef _WIN32
#include <sys/resource.h>
#endif
extern void *retcon_leaf(void);
extern void *retcon_value(void);
extern void *retcon_branch(void *);
extern void *retcon_shared(void *);
extern uint64_t retcon_consume(void *);
extern void *retcon_atomic_leaf(void);
extern void *retcon_atomic_branch(void *);
extern void *retcon_mutex(void *, void *);
extern void *retcon_flat(void *, void *);
extern void *retcon_rw(void *, void *);
extern void *retcon_forest_end(void);
extern void *retcon_forest_branch(void *);
extern uint64_t retcon_forest_consume(void *);
extern void *retcon_chain(void *);
extern uint64_t retcon_chain_consume(void *);
int main(void) {
#ifndef _WIN32
  struct rlimit limit;
  assert(!getrlimit(RLIMIT_STACK, &limit));
  limit.rlim_cur = 256 * 1024;
  assert(!setrlimit(RLIMIT_STACK, &limit));
#endif
  assert(retcon_consume(retcon_leaf()) == 0);
  assert(retcon_consume(retcon_value()) == 1);
  void *(*locks[])(void *, void *) = {retcon_mutex, retcon_flat, retcon_rw};
  for (unsigned kind = 0; kind < 3; ++kind) {
    void *normal = retcon_leaf(), *atomic = retcon_atomic_leaf();
    for (unsigned i = 0; i < 100000; ++i) {
      normal = i % 2 ? retcon_branch(normal) : retcon_shared(normal);
      atomic = retcon_atomic_branch(atomic);
    }
    assert(retcon_consume(locks[kind](atomic, normal)) == 1);
  }
  void *forest = retcon_forest_end(), *chain = 0;
  for (unsigned i = 0; i < 100000; ++i) {
    forest = retcon_forest_branch(forest);
    chain = retcon_chain(chain);
  }
  assert(retcon_forest_consume(forest) == 1);
  assert(retcon_chain_consume(chain) == 42);
}

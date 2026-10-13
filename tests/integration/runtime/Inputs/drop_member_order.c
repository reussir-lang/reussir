#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#ifndef _WIN32
#include <sys/resource.h>
#endif

extern void *order_cons(void *, uint64_t);
extern void *order_value_cons(void *, uint64_t);
extern void *order_nullable_cons(void *, uint64_t);
extern uint64_t order_consume(void *);
extern uint64_t order_value_consume(void *);
extern uint64_t order_nullable_consume(void *);

static const uint64_t depth = 100000;
static uint64_t seen;
static unsigned mixed;

void observe_drop(uint64_t id) {
  uint64_t expected = seen;
  if (mixed)
    expected = seen < depth ? 2 * (depth - seen - 1) : 2 * (seen - depth) + 1;
  assert(id == expected);
  ++seen;
}

// The opaque FFI handles are only moved; their custom hook owns all cleanup.
void *make_handle(uint64_t id) {
  uint64_t *handle = malloc(sizeof(*handle));
  assert(handle);
  *handle = id;
  return handle;
}
void drop_handle(void *handle) {
  observe_drop(*(uint64_t *)handle);
  free(handle);
}

int main(void) {
#ifndef _WIN32
  struct rlimit limit;
  assert(!getrlimit(RLIMIT_STACK, &limit));
  limit.rlim_cur = 256 * 1024;
  assert(!setrlimit(RLIMIT_STACK, &limit));
#endif
  void *(*constructors[])(void *, uint64_t) = {order_cons, order_value_cons,
                                               order_nullable_cons};
  uint64_t (*consumers[])(void *) = {order_consume, order_value_consume,
                                     order_nullable_consume};
  for (unsigned kind = 0; kind < 3; ++kind) {
    void *chain = 0;
    mixed = kind == 0;
    seen = 0;
    for (uint64_t i = 0; i < depth; ++i)
      chain = constructors[kind](chain, i);
    assert(seen == 0);
    assert(consumers[kind](chain) == 0);
    assert(seen == depth * (mixed ? 2 : 1));
  }
}

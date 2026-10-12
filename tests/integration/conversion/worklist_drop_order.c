#include <stdint.h>
#include <stdlib.h>
#include <sys/resource.h>

extern void *order_make(void *, void *, void *);
extern void order_drop(void **);
#ifdef SHALLOW
static const uintptr_t count = 32;
#else
static const uintptr_t count = 100000;
#endif
static uintptr_t *events;
static size_t length;
static const uintptr_t nested_pre = UINTPTR_MAX - 1;
static const uintptr_t nested_post = UINTPTR_MAX;

void order_note(void *object) {
  uintptr_t value = (uintptr_t)object;
  events[length++] = value;
  if (value == 2 * count - 1) {
    void *nested = order_make(0, (void *)nested_pre, (void *)nested_post);
    order_drop(&nested);
  }
}
int main(void) {
  struct rlimit limit;
  if (getrlimit(RLIMIT_STACK, &limit))
    return 1;
  limit.rlim_cur = 256 * 1024;
  if (setrlimit(RLIMIT_STACK, &limit))
    return 2;
  events = calloc(2 * count + 2, sizeof(*events));
  if (!events)
    return 3;
  void *root = 0;
  for (uintptr_t i = 1; i <= count; ++i)
    root = order_make(root, (void *)(2 * i - 1), (void *)(2 * i));
  order_drop(&root);
  if (length != 2 * count + 2 || events[0] != 2 * count - 1 ||
      events[1] != nested_pre || events[2] != nested_post)
    return 4;
  size_t pos = 3;
  for (uintptr_t i = count - 1; i > 0; --i)
    if (events[pos++] != 2 * i - 1)
      return 5;
  for (uintptr_t i = 1; i <= count; ++i)
    if (events[pos++] != 2 * i)
      return 6;
  free(events);
  return 0;
}

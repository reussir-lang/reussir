#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void *__reussir_allocate(size_t, size_t);
extern void __reussir_deallocate(void *, size_t, size_t);
extern void order_drop_leaf(void *);
extern void order_drop_node(void *);
extern void order_drop_after(void *);
extern void order_drop_before(void *);

typedef struct Handle {
  uint32_t count;
  uint64_t id;
} Handle;
typedef struct Leaf {
  uint32_t count;
  Handle *handle;
} Leaf;
typedef struct Node {
  uint32_t count;
  Leaf *left;
  Handle *middle;
  Leaf *right;
} Node;
typedef struct Y {
  uint32_t count;
  Leaf *x;
  Handle *handle;
} Y;
typedef struct Pair {
  uint32_t count;
  void *a;
  void *b;
} Pair;

_Static_assert(sizeof(Leaf) == 16 && offsetof(Leaf, handle) == 8,
               "Leaf layout");
_Static_assert(sizeof(Node) == 32 && offsetof(Node, left) == 8, "Node layout");
_Static_assert(sizeof(Y) == 24 && sizeof(Pair) == 24, "Alias layouts");

static unsigned events[16];
static size_t event_count;
static Leaf *nested_leaf;

static void *allocate(size_t size) {
  void *box = __reussir_allocate(8, size);
  memset(box, 0, size);
  *(uint32_t *)box = 1;
  return box;
}
static void record(unsigned id) {
  if (event_count == sizeof(events) / sizeof(events[0]))
    abort();
  events[event_count++] = id;
}
void order_handle_drop(void *box) {
  Handle *handle = box;
  if (--handle->count)
    return;
  record(handle->id);
  if (handle->id == 20 && nested_leaf) {
    Leaf *leaf = nested_leaf;
    nested_leaf = NULL;
    order_drop_leaf(leaf);
    record(21);
  }
  __reussir_deallocate(handle, 8, sizeof(*handle));
}
static Handle *make_handle(unsigned id) {
  Handle *handle = allocate(sizeof(*handle));
  handle->id = id;
  return handle;
}
static Leaf *make_leaf(unsigned id) {
  Leaf *leaf = allocate(sizeof(*leaf));
  leaf->handle = make_handle(id);
  return leaf;
}
static Node *make_node(unsigned middle) {
  Node *node = allocate(sizeof(*node));
  node->left = make_leaf(1);
  node->middle = make_handle(middle);
  node->right = make_leaf(3);
  return node;
}
// The root and Y both own W; their decrements precede deferred cleanup.
static Pair *make_alias(int before) {
  Leaf *w = make_leaf(10);
  w->count = 2;
  Y *y = allocate(sizeof(*y));
  y->x = w;
  y->handle = make_handle(20);
  Pair *pair = allocate(sizeof(*pair));
  pair->a = before ? (void *)y : (void *)w;
  pair->b = before ? (void *)w : (void *)y;
  return pair;
}
static int expect(const char *name, const unsigned *expected, size_t count) {
  int matches = event_count == count;
  for (size_t i = 0; matches && i < count; ++i)
    matches = events[i] == expected[i];
  if (!matches) {
    fprintf(stderr, "%s:", name);
    for (size_t i = 0; i < event_count; ++i)
      fprintf(stderr, " %u", events[i]);
    fputc('\n', stderr);
  }
  event_count = 0;
  return matches;
}
int main(int argc, char **argv) {
  int backward = argc == 2 && strcmp(argv[1], "backward") == 0;
  const unsigned forward_node[] = {1, 2, 3}, backward_node[] = {3, 2, 1};
  const unsigned forward_alias[] = {10, 20}, backward_alias[] = {20, 10};
  const unsigned forward_nested[] = {1, 20, 9, 21, 3};
  const unsigned backward_nested[] = {3, 20, 9, 21, 1};
  order_drop_node(make_node(2));
  if (!expect("node", backward ? backward_node : forward_node, 3))
    return 1;
  order_drop_after(make_alias(0));
  if (!expect("alias after", backward ? backward_alias : forward_alias, 2))
    return 2;
  order_drop_before(make_alias(1));
  if (!expect("alias before", backward_alias, 2))
    return 3;
  // The public nested drop must finish before its caller records 21.
  nested_leaf = make_leaf(9);
  order_drop_node(make_node(20));
  return expect("nested public drop",
                backward ? backward_nested : forward_nested, 5)
             ? 0
             : 4;
}

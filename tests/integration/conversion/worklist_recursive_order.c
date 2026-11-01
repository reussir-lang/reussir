#include <stdint.h>
#include <stdlib.h>
#include <string.h>

extern void *__reussir_allocate(size_t, size_t);
extern void __reussir_deallocate(void *, size_t, size_t);
extern void recursive_drop(void *);

typedef struct Handle {
  uint32_t count;
  unsigned id;
} Handle;
typedef struct Node {
  uint32_t count;
  Handle *handle;
  struct Node *left, *right;
} Node;
_Static_assert(sizeof(Node) == 32, "Node layout");
static unsigned events[3], count;

void recursive_cleanup(void *box) {
  Handle *handle = box;
  if (--handle->count)
    return;
  if (handle->id) {
    if (count >= 3)
      abort();
    events[count++] = handle->id;
  }
  __reussir_deallocate(handle, 8, sizeof(*handle));
}
static Node *node(unsigned id, Node *left, Node *right) {
  Handle *handle = __reussir_allocate(8, sizeof(*handle));
  *handle = (Handle){1, id};
  Node *result = __reussir_allocate(8, sizeof(*result));
  *result = (Node){1, handle, left, right};
  return result;
}
int main(int argc, char **argv) {
  if (argc != 2)
    return 1;
  recursive_drop(node(1, node(2, NULL, NULL), node(3, NULL, NULL)));
  unsigned expected[] = {1, 2, 3};
  if (strcmp(argv[1], "backward") == 0) {
    expected[0] = 3;
    expected[2] = 1;
  }
  if (count != 3 || memcmp(events, expected, sizeof(events)))
    return 2;
  Node *chain = NULL;
  for (unsigned i = 0; i != 10000; ++i)
    chain = node(0, chain, NULL);
  recursive_drop(chain);
  return 0;
}

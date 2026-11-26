#include <stdint.h>
#include <stdlib.h>
#include <string.h>

extern void *__reussir_allocate(size_t, size_t);
extern void __reussir_deallocate(void *, size_t, size_t);
extern void pure_drop(void *);

// A handle with its id, or the box that owns a handle.
typedef struct Cell {
  uint32_t count;
  uintptr_t value;
} Cell;
typedef struct Node {
  uint32_t count;
  Cell *owner;
  struct Node *left, *right;
} Node;
static uintptr_t events[4], count;

void pure_cleanup(void *box) {
  Cell *handle = box;
  if (--handle->count)
    return;
  if (handle->value) {
    if (count >= 4)
      abort();
    events[count++] = handle->value;
  }
  __reussir_deallocate(handle, 8, sizeof(*handle));
}
static Cell *cell(uintptr_t value) {
  Cell *result = __reussir_allocate(8, sizeof(*result));
  *result = (Cell){1, value};
  return result;
}
static Node *node(unsigned id, Node *left, Node *right) {
  Node *result = __reussir_allocate(8, sizeof(*result));
  *result = (Node){1, cell((uintptr_t)cell(id)), left, right};
  return result;
}
int main(int argc, char **argv) {
  if (argc != 2)
    return 1;
  // Both children of the third node hold the fourth.
  Node *shared = node(4, NULL, NULL);
  shared->count = 2;
  pure_drop(node(1, node(2, NULL, NULL), node(3, shared, shared)));
  uintptr_t expected[] = {1, 2, 3, 4};
  if (strcmp(argv[1], "backward") == 0)
    memcpy(expected, (uintptr_t[]){4, 3, 2, 1}, sizeof(expected));
  if (count != 4 || memcmp(events, expected, sizeof(events)))
    return 2;
  Node *chain = NULL;
  for (unsigned i = 0; i != 100000; ++i)
    chain = node(0, NULL, chain);
  pure_drop(chain);
  return 0;
}

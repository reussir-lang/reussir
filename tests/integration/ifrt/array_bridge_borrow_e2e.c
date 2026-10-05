// Part of the Reussir Project, dual licensed under Apache-2.0 OR MIT.
// See LICENSE for license information.

#include <assert.h>
#include <stdint.h>

struct Box {
  uint32_t count;
  struct {
    void *handles[2];
    intptr_t offset;
    intptr_t extents[2];
  } array;
};

extern void borrow(struct Box *box);

int main(int argc, char **argv) {
  (void)argv;
  int first, second;
  struct Box box = {7, {{&first, &second}, argc > 1 ? 1 : 0, {6, 0}}};
  borrow(&box);
  if (argc > 1)
    return 0;
  assert(box.count == 7);
  assert(box.array.handles[0] == &first);
  assert(box.array.handles[1] == &second);
  assert(box.array.offset == 0);
  assert(box.array.extents[0] == 6);
  assert(box.array.extents[1] == 0);
  return 0;
}

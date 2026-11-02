#include <stdint.h>
#include <stdio.h>

extern int64_t last_use(int64_t n, int64_t key);

extern int64_t shared_lookup(int64_t n, int64_t key);
extern int64_t last_rebuild(int64_t n, int64_t key);

int main(void) {
  int64_t size = 100000;
  if (last_use(size, size) != size * 3 || last_use(size, 1) != 3 ||
      last_use(size, -1) != -1 || last_use(0, 1) != -1 ||
      last_rebuild(size, size) != size * 3 || last_rebuild(size, 1) != 3 ||
      shared_lookup(size, size) != size * 6 || shared_lookup(size, -1) != -2) {
    fputs("last-use lookup failed\n", stderr);
    return 1;
  }
  return 0;
}

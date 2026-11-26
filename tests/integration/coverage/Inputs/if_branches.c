#include <stdint.h>

int64_t if_branches_pick(int64_t);
int64_t if_branches_sign(int64_t);
int64_t if_branches_both(int64_t, int64_t);
int64_t if_branches_either(int64_t, int64_t);

int main(void) {
  int64_t sum = if_branches_pick(5) + if_branches_pick(-3) + if_branches_pick(2);
  sum += if_branches_sign(-5) + if_branches_sign(0) + if_branches_sign(7);
  sum += if_branches_both(1, 1) + if_branches_both(1, 0) + if_branches_both(0, 1);
  sum += if_branches_either(1, 0) + if_branches_either(0, 1) +
         if_branches_either(0, 0);
  return sum == 13 ? 0 : 1;
}

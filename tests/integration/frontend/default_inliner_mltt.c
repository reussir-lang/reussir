#include <stdint.h>
#include <stdio.h>

extern int64_t mltt_check_case(int64_t case_id, int64_t size, _Bool bad);
extern int64_t mltt_regressions(void);

int main(void) {
  if (mltt_regressions() != 10) {
    fputs("MLTT kernel regression failed\n", stderr);
    return 1;
  }
  // Exercise both equality-chain parities and a large standalone proof bundle.
  int64_t sizes[] = {1, 1024};
  for (unsigned i = 0; i < sizeof(sizes) / sizeof(sizes[0]); ++i) {
    for (int64_t id = 0; id < 3; ++id) {
      int64_t valid = mltt_check_case(id, sizes[i], 0);
      int64_t invalid = mltt_check_case(id, sizes[i], 1);
      if (valid != 1 || invalid != 0) {
        fprintf(stderr,
                "MLTT case %lld size %lld: expected 1/0, got %lld/%lld\n",
                (long long)id, (long long)sizes[i], (long long)valid,
                (long long)invalid);
        return 1;
      }
    }
  }
  return 0;
}

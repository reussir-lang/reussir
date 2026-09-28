#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

// Must match reussir_rt::pjrt::ffi. These are Reussir's compact descriptors,
// independent of the versioned PJRT C structs constructed inside the runtime.
struct AllocationOptions {
  const char *memory_kind;
  size_t memory_kind_size;
  const int64_t *minor_to_major;
  const int64_t *tile_dims;
  const size_t *tile_dim_sizes;
  size_t num_tiles;
};
struct HostLayout {
  size_t rank;
  const int64_t *byte_strides;
  const int64_t *minor_to_major;
  const int64_t *tile_dims;
  const size_t *tile_dim_sizes;
  size_t num_tiles;
};
extern size_t transfer(const void *, const int64_t *, const int64_t *,
                       const struct AllocationOptions *, const struct HostLayout *,
                       void *);

int main(int argc, char **argv) {
  const int32_t input[] = {0, 1, 2, 3, 4, 5};
  const int64_t dims[] = {2, 3}, strides[] = {4, -8};
  const int64_t row[] = {1, 0}, column[] = {0, 1};
  struct AllocationOptions options = {
      .memory_kind = "device", .memory_kind_size = 6, .minor_to_major = row};
  struct HostLayout layout = {.rank = 2, .minor_to_major = column};
  if (argc > 1 && strcmp(argv[1], "bad-layout") == 0)
    layout.byte_strides = strides;
  if (argc > 1 && strcmp(argv[1], "bad-memory") == 0) {
    options.memory_kind = "missing";
    options.memory_kind_size = 7;
  }
  // Guards detect writes beyond the exact size reported for this host layout.
  int32_t output[] = {-77, 0, 0, 0, 0, 0, 0, -77};
  assert(transfer(input + 4, dims, strides, &options, &layout, output + 1) == 24);
  const int32_t expected[] = {-77, 4, 5, 2, 3, 0, 1, -77};
  assert(memcmp(output, expected, sizeof(output)) == 0);
  layout.minor_to_major = row;
  assert(transfer(input + 4, dims, strides, &options, &layout, output + 1) == 24);
  const int32_t row_expected[] = {-77, 4, 2, 0, 5, 3, 1, -77};
  assert(memcmp(output, row_expected, sizeof(output)) == 0);
  return 0;
}

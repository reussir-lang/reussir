#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// String struct matching LLVM lowering of !reussir.str<local>
typedef struct {
  const char *ptr;
  size_t len;
} ReussirStr;

// Result struct for (index, i1) return type
// LLVM packs this as { i64, i1 } which in C ABI becomes two return values
// or a struct return depending on platform - we use the struct approach
typedef struct {
  size_t index;
  uint8_t found;
} SelectResult;

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wgcc-compat"
#if defined(_WIN32) && defined(__x86_64__)
#define API_FLAG [[gnu::sysv_abi]]
#else
#define API_FLAG
#endif
// Declare the MLIR-generated functions
extern SelectResult test_basic(ReussirStr str) API_FLAG;
extern SelectResult test_prefix(ReussirStr str) API_FLAG;
extern SelectResult test_long(ReussirStr str) API_FLAG;
extern SelectResult test_many(ReussirStr str) API_FLAG;
#pragma clang diagnostic pop

// Helper to create a ReussirStr from a C string
static ReussirStr make_str(const char *s) {
  ReussirStr str;
  str.ptr = s;
  str.len = strlen(s);
  return str;
}

// Test assertion macro
#define ASSERT(cond)                                                           \
  do {                                                                         \
    if (!(cond)) {                                                             \
      abort();                                                                 \
    }                                                                          \
  } while (0)
#define ASSERT_MATCH(result, expected_idx)                                     \
  ASSERT(result.found &&result.index == (expected_idx))
#define ASSERT_NO_MATCH(result) ASSERT(!result.found)

int main() {
  SelectResult r;

  // Test basic pattern matching
  r = test_basic(make_str("foo"));
  ASSERT_MATCH(r, 0);

  r = test_basic(make_str("bar"));
  ASSERT_MATCH(r, 1);

  r = test_basic(make_str("baz"));
  ASSERT_NO_MATCH(r);

  r = test_basic(make_str("foobar"));
  ASSERT_NO_MATCH(r); // "foobar" is not exactly "foo"

  r = test_basic(make_str(""));
  ASSERT_NO_MATCH(r);

  // Test prefix patterns (distinguishing "abc" vs "abd")
  r = test_prefix(make_str("abc"));
  ASSERT_MATCH(r, 0);

  r = test_prefix(make_str("abd"));
  ASSERT_MATCH(r, 1);

  r = test_prefix(make_str("xyz"));
  ASSERT_MATCH(r, 2);

  r = test_prefix(make_str("abz"));
  ASSERT_NO_MATCH(r);

  r = test_prefix(make_str("ab"));
  ASSERT_NO_MATCH(r); // prefix of pattern but not full match

  // Test long pattern
  r = test_long(
      make_str("extremely_long_unique_pattern_that_is_over_32_chars"));
  ASSERT_MATCH(r, 0);

  r = test_long(make_str("extremely_long_unique_pattern"));
  ASSERT_NO_MATCH(r); // partial match

  r = test_long(make_str("short"));
  ASSERT_NO_MATCH(r);

  // Test many patterns
  r = test_many(make_str("apple"));
  ASSERT_MATCH(r, 0);

  r = test_many(make_str("banana"));
  ASSERT_MATCH(r, 1);

  r = test_many(make_str("cherry"));
  ASSERT_MATCH(r, 2);

  r = test_many(make_str("date"));
  ASSERT_MATCH(r, 3);

  r = test_many(make_str("elderberry"));
  ASSERT_MATCH(r, 4);

  r = test_many(make_str("fig"));
  ASSERT_NO_MATCH(r);

  return 0;
}

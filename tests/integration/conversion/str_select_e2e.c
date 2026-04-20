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

typedef struct {
  ReussirStr str;
} SelectArgs;

extern void test_basic(SelectResult *out, SelectArgs *args);
extern void test_prefix(SelectResult *out, SelectArgs *args);
extern void test_long(SelectResult *out, SelectArgs *args);
extern void test_many(SelectResult *out, SelectArgs *args);

// Helper to create a ReussirStr from a C string
static ReussirStr make_str(const char *s) {
  ReussirStr str;
  str.ptr = s;
  str.len = strlen(s);
  return str;
}

static SelectResult call_select(void (*fn)(SelectResult *, SelectArgs *),
                                const char *s) {
  SelectResult out;
  SelectArgs args;
  args.str = make_str(s);
  fn(&out, &args);
  return out;
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
  r = call_select(test_basic, "foo");
  ASSERT_MATCH(r, 0);

  r = call_select(test_basic, "bar");
  ASSERT_MATCH(r, 1);

  r = call_select(test_basic, "baz");
  ASSERT_NO_MATCH(r);

  r = call_select(test_basic, "foobar");
  ASSERT_NO_MATCH(r); // "foobar" is not exactly "foo"

  r = call_select(test_basic, "");
  ASSERT_NO_MATCH(r);

  // Test prefix patterns (distinguishing "abc" vs "abd")
  r = call_select(test_prefix, "abc");
  ASSERT_MATCH(r, 0);

  r = call_select(test_prefix, "abd");
  ASSERT_MATCH(r, 1);

  r = call_select(test_prefix, "xyz");
  ASSERT_MATCH(r, 2);

  r = call_select(test_prefix, "abz");
  ASSERT_NO_MATCH(r);

  r = call_select(test_prefix, "ab");
  ASSERT_NO_MATCH(r); // prefix of pattern but not full match

  // Test long pattern
  r = call_select(test_long,
                  "extremely_long_unique_pattern_that_is_over_32_chars");
  ASSERT_MATCH(r, 0);

  r = call_select(test_long, "extremely_long_unique_pattern");
  ASSERT_NO_MATCH(r); // partial match

  r = call_select(test_long, "short");
  ASSERT_NO_MATCH(r);

  // Test many patterns
  r = call_select(test_many, "apple");
  ASSERT_MATCH(r, 0);

  r = call_select(test_many, "banana");
  ASSERT_MATCH(r, 1);

  r = call_select(test_many, "cherry");
  ASSERT_MATCH(r, 2);

  r = call_select(test_many, "date");
  ASSERT_MATCH(r, 3);

  r = call_select(test_many, "elderberry");
  ASSERT_MATCH(r, 4);

  r = call_select(test_many, "fig");
  ASSERT_NO_MATCH(r);

  return 0;
}

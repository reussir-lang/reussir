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

// Platform-specific calling convention handling
#if defined(_WIN32) && defined(__x86_64__)
// On Windows x86_64, the assembly uses:
// - Input:  %rcx = ptr, %rdx = len (Windows x64 ABI)
// - Output: %rax = index, %rdx = found (custom two-register return)
// We declare raw symbol addresses and use inline asm to call them properly.
extern void test_basic(void);
extern void test_prefix(void);
extern void test_long(void);
extern void test_many(void);

// Wrapper that calls the function and captures both %rax and %rdx return
// registers
static inline SelectResult call_select_func(void (*func)(void), const char *ptr,
                                            size_t len) {
  size_t index;
  uint8_t found;
  __asm__ __volatile__("callq *%[func]"
                       : "=a"(index), "=d"(found)
                       : [func] "r"(func), "c"(ptr), "d"(len)
                       : "r8", "r9", "r10", "r11", "memory");
  SelectResult res = {index, found};
  return res;
}

// Wrapper functions to translate calling conventions
static SelectResult test_basic_wrapper(ReussirStr str) {
  return call_select_func(test_basic, str.ptr, str.len);
}
static SelectResult test_prefix_wrapper(ReussirStr str) {
  return call_select_func(test_prefix, str.ptr, str.len);
}
static SelectResult test_long_wrapper(ReussirStr str) {
  return call_select_func(test_long, str.ptr, str.len);
}
static SelectResult test_many_wrapper(ReussirStr str) {
  return call_select_func(test_many, str.ptr, str.len);
}

// Macros to redirect calls to wrappers
#define test_basic(str) test_basic_wrapper(str)
#define test_prefix(str) test_prefix_wrapper(str)
#define test_long(str) test_long_wrapper(str)
#define test_many(str) test_many_wrapper(str)
#else
// Non-Windows platforms use direct struct return
extern SelectResult test_basic(ReussirStr str);
extern SelectResult test_prefix(ReussirStr str);
extern SelectResult test_long(ReussirStr str);
extern SelectResult test_many(ReussirStr str);
#endif

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

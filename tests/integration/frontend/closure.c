#include <stdint.h>
#include <stdlib.h>

extern int64_t test_rc_closure_ffi(void);
extern int64_t test_rc_arg_closure_ffi(void);

int main(void) {
    // Closure capturing an RC value: should return 42
    if (test_rc_closure_ffi() != 42)
        abort();

    // Closure with an RC argument: should return 100
    if (test_rc_arg_closure_ffi() != 100)
        abort();

    return 0;
}

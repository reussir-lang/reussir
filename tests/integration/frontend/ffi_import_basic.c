#include <stdio.h>
#include <stdint.h>

// Provided by the Reussir module
int32_t _RC11use_add_one(int32_t x);

// Imported by the Reussir module - we provide it here
int32_t _RC7add_one(int32_t x) {
    return x + 1;
}

int main(void) {
    int32_t result = _RC11use_add_one(41);
    if (result != 42) {
        printf("FAIL: expected 42, got %d\n", result);
        return 1;
    }
    return 0;
}

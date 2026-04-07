#include <stdio.h>
#include <stdint.h>

// Exported trampolines from Reussir
int32_t add_i32(int32_t x, int32_t y);
double add_f64(double x, double y);

int main(void) {
    int32_t r1 = add_i32(20, 22);
    if (r1 != 42) {
        printf("FAIL: add_i32(20, 22) = %d, expected 42\n", r1);
        return 1;
    }

    double r2 = add_f64(1.5, 2.5);
    if (r2 < 3.9 || r2 > 4.1) {
        printf("FAIL: add_f64(1.5, 2.5) = %f, expected 4.0\n", r2);
        return 1;
    }

    return 0;
}

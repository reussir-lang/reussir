#include <stdint.h>

extern int64_t reussir_main(void);

int main(void) { return reussir_main() == 573 ? 0 : 1; }

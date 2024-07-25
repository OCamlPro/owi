#include <owi.h>

int main(void) {
    char c = owi_char();
    owi_assume(c == 'B');
    owi_assert('b' - c == 32);
    return 0;
}

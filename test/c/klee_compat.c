#include <klee/klee.h>

int main(void) {
    char c;
    klee_make_symbolic(&c, sizeof(c), "char");
    klee_assume(c == 'B');
    klee_assert('b' - c == 32);

    int n = klee_int("int");
    klee_assume(n > 0);
    klee_assert(n + 1 > 0);

    int n_range = klee_range(-10, 10, "range");
    klee_assert(n_range >= -10);
    klee_assert(n_range < 10);
    return 0;
}
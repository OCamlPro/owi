#include <owi.h>

int col;
//@ global invariant I : 0 <= col <= 100;

void increment(void) {
    col += 1;
}

int main(void) {
    col = owi_i32();
    owi_assume(0 <= col);
    owi_assume(col <= 100);
    increment();
    return 0;
}

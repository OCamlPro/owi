#include <owi.h>

int col;
//@ strong global invariant I : 0 <= col <= 100;

void not_increment(void) {
    col += 1;
    col -= 1;
}

int main(void) {
    col = owi_i32();
    owi_assume(0 <= col);
    owi_assume(col <= 100);
    not_increment();
    return 0;
}

#include <owi.h>

int main(void) {
    int x = owi_int();
    owi_assume(x >= 0);
    owi_assume(x <= 20);
    while (x > 0) {
    /*@ continues x % 7 == 0  && (x + 1) % 11 != 0 && x == \old(x) - 1;
      @ ensures   (x + 1) % 5 != 0 && (x + 2) % 7 != 0  && (x + 3) % 11 != 0 && x == \old(x) - 3;
    */
    {
        if (x % 11 == 0) break;
        x--;
        if (x % 7 == 0) continue;
        x--;
        if (x % 5 == 0) return x;
        x--;
    }
    }
    return 0;
}

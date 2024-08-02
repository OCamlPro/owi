// example from page 43 of ACSL language reference

#include <owi.h>

struct intpair {
    int x, y;
};

/*@ predicate lexico(struct intpair p1, struct intpair p2) =
    (p1.x >  p2.x && p1.x >= 0) ||
    (p1.x == p2.x && p1.y >  p2.y && p1.y >= 0);
*/

//@ requires x >= 0 && y >= 0;
void f(int x, int y) {
    /*@ ghost
    struct intpair m;
    m.x = x;
    m.y = y;
    */

    /*@ loop invariant x >= 0 && y >= 0;
        loop variant   m for lexico
    */
    while(x > 0 && y > 0) {
        if (y > 0) y -= 1;
        else x -= 1;
        /*@ ghost
        m.x = x;
        m.y = y;
        */
    }
}

int main(void) {
    int x = owi_i32(), y = owi_i32();
    owi_assume(x >= 0);
    owi_assume(y >= 0);
    f(x, y);
    return 0;
}

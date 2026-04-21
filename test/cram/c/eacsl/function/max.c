#include <owi.h>

/*@ behavior left:
        assumes x >= y;
        ensures \result == x;
    behavior right:
        assumes x <= y;
        ensures \result == y;

    disjoint behaviors;
    complete behaviors;
*/
int max(int x, int y) {
    return (x > y) ? x : y;
}

int main(void) {
    int x = owi_int();
    int y = owi_int();
    max(x, y);
    return 0;
}

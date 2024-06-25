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
    int x = owi_i32();
    int y = owi_i32();
    max(x, y);
    return 0;
}
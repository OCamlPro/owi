#include <limits.h>
#include <owi.h>

/*@
    requires value > INT_MIN;
    ensures \result == value;

    behavior neg:
        assumes value < 0;
        requires value < 1;
        ensures \result == value;

    behavior pos:
        assumes value >= 0;
        requires value > -1;
        ensures \result == value;

    behavior odd:
        assumes value % 2 == 1 || value % 2 == -1;
        requires (value % 2) - 1 == 0 || (value % 2) + 1 == 0;
        ensures \result == value;

    behavior even:
        assumes value % 2 == 0;
        requires (value % 2) + 1 == 1;
        ensures \result == value;

    complete behaviors neg, pos;
    complete behaviors odd, even;
    complete behaviors;

    disjoint behaviors neg, pos;
    disjoint behaviors odd, even;
*/
int f(int value) {
    return value;
}

int main() {
    int value = owi_int();
    owi_assume(value > INT_MIN);
    f(value);
    return 0;
}

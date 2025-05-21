#include <owi.h>

int main(void) {
    int n = owi_int();
    //@ ghost int is_case2 = 0;
    switch (n) {
        case 0: break;
        case 1: break;
        /*@ ghost
        case 2: {
            is_case2 = 1;
            // break; this break is not controlled
        }
        */
        default: break;
    }
    //@ assert is_case2 == 0;
    return 0;
}

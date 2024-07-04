#include <owi.h>

int main(void) {
    int n = owi_i32();
    //@ ghost int is_default = 0;
    switch (n) {
        case 0: {
            break;
        }
        case 1: {
            break;
        }
        /*@ ghost default: {
            is_default = 1;
            break;
        }
        */
    }
    //@ assert is_default == 0;
    return 0;
}

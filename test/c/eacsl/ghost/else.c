#include <owi.h>

int main(void) {
    int x = owi_i32();
    owi_assume(x >= -1);

    //@ ghost int num;

    if (x >= 0) {
        //@ ghost num = 42;
    }
    //@ ghost else num = 0;

   //@ assert num == 42;

    return 0;
}

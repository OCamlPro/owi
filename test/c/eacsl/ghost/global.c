#include <owi.h>

//@ ghost int cnt;

/*@ ensures cnt > 10;
    ensures cnt < 20;
*/
void loop(int n) {
    //@ ghost cnt = 0;
    while (n--) {
        //@ ghost cnt += 1;
    }
}

int main(void) {
    int n = owi_i32();
    owi_assume(n > 10);
    owi_assume(n <= 20);
    loop(n);
    return 0;
}

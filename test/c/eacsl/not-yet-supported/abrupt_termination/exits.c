#include <owi.h>
#include <stdlib.h>

int status = 0;

/*@ exits  !cond && status == val && \exit_status == 0;
    ensures cond && status == \old(status);
*/
void may_exit(int cond, int val) {
    if (!cond) {
        status = val;
        exit(0);
    }
}

int main(void) {
    int cond = owi_int(), val = owi_int();
    may_exit(cond, val);
    return 0;
}

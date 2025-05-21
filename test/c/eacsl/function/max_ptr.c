#include <owi.h>
#include <stdlib.h>

/*@ requires \valid(p) && \valid(q);
    ensures  *p < *q;
*/
void max_ptr(int *p, int *q) {
    if (*p > *q) {
        int tmp = *p;
        *p = *q;
        *q = tmp;
    }
}

int main() {
    int* p;
    int* q;
    p = (int*)malloc(sizeof(int));
    q = (int*)malloc(sizeof(int));
    *p = owi_int();
    *q = owi_int();
    max_ptr(p, q);
    return 0;
}

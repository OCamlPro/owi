#include <owi.h>

int main() {
    int i = owi_range(10, 100);
    owi_assume(i > 48);
    owi_assume(i < 51);
    if (i == 50) {
        owi_label_set(1, "label_1");
    } else {
        owi_label_set(2, "label_2");
    }
    owi_assert(0);
    return 0;
}

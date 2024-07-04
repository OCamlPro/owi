#include <owi.h>
#include <stddef.h>
#include <stdlib.h>

typedef struct List {
    int element;
    struct List* next;
} List;

void traverse(List* node) /*@ ghost (int length) */ {
    /*@ loop variant length;
    */
    while (node != NULL) {
        node = node->next;
        //@ ghost length --;
    }
}

List* makeList(int element, List* next) {
    List *node = (List *)malloc(sizeof(List));
    if (node == NULL) return NULL;
    node->element = element;
    node->next = next;
    return node;
}

int size(List* node) {
    if (node == NULL) return 0;
    int cnt = 0;
    while (node != NULL) {
        node = node->next;
        cnt ++;
    }
    return cnt;
}

/*@ ghost int ghost_size(List* node) {
    if (node == NULL) return 0;
    int cnt = 0;
    while (node != NULL) {
        node = node->next;
        cnt ++;
    }
    return cnt;
}*/

int main(void) {
    int n = owi_i32();
    owi_assume(n >= 10);
    owi_assume(n <= 20);
    List *node = NULL;
    while (n--) node = makeList(0, node);
    traverse(node) /*@ ghost (ghost_size(node)) */ ;
    return 0;
}

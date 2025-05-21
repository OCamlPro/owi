#include <owi.h>
#include <stddef.h>
#include <stdlib.h>

struct List {
    int element;
    struct List* next;
};
/*@ ghost struct List {
    int order;
}; */

void traverse(struct List* node) /*@ ghost (int length) */ {
    /*@ loop invariant length == node->order;
        loop variant length;
    */
    while (node != NULL) {
        node = node->next;
        //@ ghost length --;
    }
}

struct List* makeList(int element, struct List* next) {
    struct List *node = (struct List *)malloc(sizeof(struct List));
    if (node == NULL) return NULL;
    node->element = element;
    node->next = next;
    /*@ ghost
    if (node->next == NULL) node->order = 1;
    else node->order = node->next->order + 1; */
    return node;
}

int size(struct List* node) {
    if (node == NULL) return 0;
    int cnt = 0;
    while (node != NULL) {
        node = node->next;
        cnt ++;
    }
    return cnt;
}

/*@ ghost int ghost_size(struct List* node) {
    if (node == NULL) return 0;
    int cnt = 0;
    while (node != NULL) {
        node = node->next;
        cnt ++;
    }
    return cnt;
} */

int main(void) {
    int n = owi_int();
    owi_assume(n >= 10);
    owi_assume(n <= 20);
    struct List *node = NULL;
    while (n--) node = makeList(0, node);
    //@ ghost int length = ghost_size(node);
    traverse(node) /*@ ghost (length) */ ;
    return 0;
}

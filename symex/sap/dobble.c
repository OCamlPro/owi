// An encoding representing the problem of finding a suitable
// set of cards for https://en.wikipedia.org/wiki/Dobble.
// Cards are encoded on integers, with each position
// representing one of N_CARDS possible symbols.
#include <owi.h>
#include <stdlib.h>

// Number of symbols per card
#define CARD_SIZE 3

#define N_CARDS ((CARD_SIZE*CARD_SIZE) - CARD_SIZE + 1)

int popcount(unsigned int x) {
    int count = 0;
    for (int i = 0; i < N_CARDS; i++) {
        count += x & 1;
        x >>= 1;
    }
    return count;
}

int main() {
    unsigned int cards[N_CARDS];
    for (int i=0;i < N_CARDS; i++) {
        unsigned int x = owi_unsigned_int();
        owi_assume((x >> N_CARDS) == 0);
        owi_assume(popcount(x) == CARD_SIZE);
        cards[i] = x;
        if (i > 0) {
            owi_assume(cards[i] > cards[i-1]);
        }
    }
    unsigned int acc = 1;
    for (int i=0;i < N_CARDS; i++) {
        for(int j=i+1; j < N_CARDS;j++) {
            owi_assume(cards[i] != cards[j]);
            unsigned int z = cards[i] & cards[j];
            acc = acc & (z != 0);
            acc = acc & ((z & (z-1)) == 0);
        }
    }
    owi_assert(!acc);
}

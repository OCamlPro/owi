#include <owi.h>
#include <gmp.h>

int main(void) {
    int owi_var;
    owi_var = owi_i32();

    mpz_t gmp_var;
    mpz_init_set_si(gmp_var, 0);

    int result = mpz_cmp_si(gmp_var, owi_var) == 0;
    owi_assert(result);

    return 0;
}
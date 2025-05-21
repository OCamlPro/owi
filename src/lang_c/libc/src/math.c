#include <math.h>

// TODO: External to OWI
double sin(double x) { return x; }
double cos(double x) { return x; }
double floor(double x) { return x; }
double round(double x) { return x; }
float powf(float x, float y) { return x; }
float floorf(float x) { return x; }
double ceil(double x) { return x; }
double sqrt(double x) { return x; }
float roundf(float x) { return x; }
float log2f(float x) { return x; }
float sqrtf(float x) { return x; }
float fabsf(float x) {
  __asm__ __volatile__(
    "local.get 0;"
    "f32.abs;"
    "return;"
  );
}
/*
int isnan(float x) {
  __asm__ __volatile__(
    "local.get 0;"
    "local.get 0;"
    "f32.ne;"
    "return;"
  );
}
*/

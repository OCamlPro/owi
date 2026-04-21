#include <owi.h>

int main() {
  char* name_x = "some_name";
  int x = owi_int(name_x);
  int y = owi_int("y");
  owi_assert(x == y);
  return 0;
}

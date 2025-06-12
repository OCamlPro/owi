# C++ Quickstart

## Checking the equivalence of two functions

<!-- $MDX file=mean.cpp -->
```cpp
#include <owi.h>

struct IntPair {
  int x, y;
  int mean1() const { return (x & y) + ((x ^ y) >> 1); }
  int mean2() const { return (x + y) / 2; }
};

int main() {
  IntPair p{owi_i32(), owi_i32()};
  owi_assert(p.mean1() == p.mean2());
}
```

```sh
$ owi c++ ./mean.cpp --no-assert-failure-expression-printing
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32 -2147483648
  symbol symbol_1 i32 -2147483646
  entry_point main
}
owi: [ERROR] Reached problem!
[13]
```

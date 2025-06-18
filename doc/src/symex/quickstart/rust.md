# Rust Quickstart

Given the following `main.rs` file:

<!-- $MDX file=mean.rs -->
```rs
use owi_sym::Symbolic;

fn mean_wrong(x: i32, y: i32) -> i32 {
    (x + y) / 2
}

fn mean_correct(x: i32, y: i32) -> i32 {
    (x & y) + ((x ^ y) >> 1)
}

fn main() {
    let x = i32::symbol();
    let y = i32::symbol();
    owi_sym::assert(mean_wrong(x, y) == mean_correct(x, y))
}
```

Let's check if the two functions are the same for any input:

```sh
$ owi rust ./mean.rs -w1 --fail-on-assertion-only --no-assert-failure-expression-printing --deterministic-result-order
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32 -487586839
  symbol symbol_1 i32 486539486
}

owi: [ERROR] Reached problem!
[13]
```

Indeed, in this case, there will be an integer overflow in one of the two functions and thus they won't give the same result.

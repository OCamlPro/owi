# Zig Quickstart

## Fibonacci example

Given the following `fib.zig` file:

<!-- $MDX file=fib.zig -->
```zig
// TODO: replace this by a proper include of the owi header?
extern "symbolic" fn i32_symbol() i32;
extern "symbolic" fn assume(bool) void;
extern "symbolic" fn assert(bool) void;

fn fibonacci(n: i32) i32 {
    if (n < 0) {
        @panic("expected a positive number");
    }
    if (n <= 2) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

pub fn main() void {
    const n: i32 = i32_symbol();
    assume(n > 0);
    assume(n < 10);
    const result = fibonacci(n);
    assert(result != 21);
}
```

Owi can find a crash with:

```sh
$ owi zig ./fib.zig -w1 --no-assert-failure-expression-printing
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32 7
  entry_point _start
}
owi: [ERROR] Reached problem!
[13]
```


# Quickstart

## Finding a simple crash in a function

{{#tabs global="lang" }}
{{#tab name="C" }}
TODO
{{#endtab }}
{{#tab name="C++" }}
TODO
{{#endtab }}
{{#tab name="Rust" }}
TODO
{{#endtab }}
{{#tab name="Zig" }}
TODO
{{#endtab }}
{{#tab name="Wasm" }}
TODO
{{#endtab }}
{{#endtabs }}

{{#tabs global="lang" }}
{{#tab name="C" }}
TODO
{{#endtab }}
{{#tab name="C++" }}
TODO
{{#endtab }}
{{#tab name="Rust" }}
TODO
{{#endtab }}
{{#tab name="Zig" }}
TODO
{{#endtab }}
{{#tab name="Wasm" }}
TODO
{{#endtab }}
{{#endtabs }}

## Defining symbols by hand

{{#tabs global="lang" }}
{{#tab name="C" }}
TODO
{{#endtab }}
{{#tab name="C++" }}
TODO
{{#endtab }}
{{#tab name="Rust" }}
TODO
{{#endtab }}
{{#tab name="Zig" }}
TODO
{{#endtab }}
{{#tab name="Wasm" }}
TODO
{{#endtab }}
{{#endtabs }}

{{#tabs global="lang" }}
{{#tab name="C" }}
TODO
{{#endtab }}
{{#tab name="C++" }}
TODO
{{#endtab }}
{{#tab name="Rust" }}
TODO
{{#endtab }}
{{#tab name="Zig" }}
TODO
{{#endtab }}
{{#tab name="Wasm" }}
TODO
{{#endtab }}
{{#endtabs }}

## Checking the equivalence of two functions

Here, we have two functions that we expect to be the same but we are not completely sure. This can be the case for instance when refactoring or optimizing a given function. Owi can check that the old one is equivalent to the new one.

We have the original function, `mean_old`, that computes the mean of two integers. Then, we define the new function, `mean_new`, which we expect to do the same. Then, our main function is creating two symbolic integers, `n1` and `n2`, and asserts that the two functions always return the same value when given these symbolic integers as input.

{{#tabs global="lang" }}
{{#tab name="C" }}
<!-- $MDX file=mean.c -->
```c
#include <owi.h>

int mean1(int x, int y) {
  return (x & y) + ((x ^ y) >> 1);
}

int mean2(int x, int y) {
  return (x + y) / 2;
}

void check(int x, int y) {
  owi_assert(mean1(x, y) == mean2(x, y));
}
```
{{#endtab }}
{{#tab name="C++" }}
<!-- $MDX file=mean.cpp -->
```cpp
#include <owi.h>

int mean1(int x, int y) {
  return (x & y) + ((x ^ y) >> 1);
}

int mean2(int x, int y) {
  return (x + y) / 2;
}

extern "C" void check(int x, int y) {
  owi_assert(mean1(x, y) == mean2(x, y));
}
```
{{#endtab }}
{{#tab name="Rust" }}
<!-- $MDX file=mean.rs -->
```rs
#![no_main]

fn mean1(x: i32, y: i32) -> i32 {
    (x & y) + ((x ^ y) >> 1)
}

fn mean2(x: i32, y: i32) -> i32 {
    (x + y) / 2
}

#[no_mangle]
pub extern "C" fn check(x : i32, y: i32) {
  owi_sym::assert(mean1(x, y) == mean2(x, y))
}
```
{{#endtab }}
{{#tab name="Zig" }}
<!-- $MDX file=mean.zig -->
```rs
extern "owi" fn i32_symbol() i32;
extern "owi" fn assert(bool) void;

fn mean1(x: i32, y: i32) i32 {
  return (x & y) + ((x ^ y) >> 1);
}

fn mean2(x: i32, y: i32) i32 {
  return @divTrunc(x + y, 2);
}

export fn check(x: i32, y: i32) void {
  assert(mean1(x, y) == mean2(x, y));
}
```
{{#endtab }}
{{#tab name="Wasm" }}
<!-- $MDX file=mean.wat -->
```wasm
(module
  (func $mean1 (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.and

    local.get $x
    local.get $y
    i32.xor
    i32.const 1
    i32.shr_s

    i32.add
  )

  (func $mean2 (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.add
    i32.const 2
    i32.div_s
  )

  (func $check  (export "check") (param $x i32) (param $y i32)
    local.get $x
    local.get $y
    call $mean1

    local.get $x
    local.get $y
    call $mean2

    i32.ne
    if
      unreachable
    end
  )
)
```
{{#endtab }}
{{#endtabs }}

We can now run Owi on our program to check if they are the same:

{{#tabs global="lang" }}
{{#tab name="C" }}
```sh
$ owi c ./mean.c --entry-point=check --invoke-with-symbols --no-assert-failure-expression-printing
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32 -990380032
  symbol symbol_1 i32 -2145908736
}
owi: [ERROR] Reached problem!
[13]
```
{{#endtab }}
{{#tab name="C++" }}
```sh
$ owi c++ ./mean.cpp --entry-point=check --invoke-with-symbols --no-assert-failure-expression-printing
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32 -990380032
  symbol symbol_1 i32 -2145908736
}
owi: [ERROR] Reached problem!
[13]
```
{{#endtab }}
{{#tab name="Rust" }}
```sh
$ owi rust ./mean.rs --entry-point=check --invoke-with-symbols -w1 --fail-on-assertion-only --no-assert-failure-expression-printing --deterministic-result-order
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32 -1052888556
  symbol symbol_1 i32 -808758763
}

owi: [ERROR] Reached problem!
[13]
```
{{#endtab }}
{{#tab name="Zig" }}
```sh
$ owi zig ./mean.zig --entry-point=check --invoke-with-symbols -w1 --fail-on-assertion-only --no-assert-failure-expression-printing --deterministic-result-order
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32 -1023139840
  symbol symbol_1 i32 -1073471487
}

owi: [ERROR] Reached problem!
[13]
```
{{#endtab }}
{{#tab name="Wasm" }}
```sh
$ owi sym ./mean.wat --entry-point=check --invoke-with-symbols
owi: [ERROR] Trap: unreachable
model {
  symbol symbol_0 i32 -2147483648
  symbol symbol_1 i32 -2147483646
}
owi: [ERROR] Reached problem!
[13]
```
{{#endtab }}
{{#endtabs }}

And indeed, in the `mean1` function, when using these values, there will be an overflow, leading to a wrong result.

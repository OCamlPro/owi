# Quickstart

## Finding a crash in a function

Let's say you wrote a function `f` and want to check if it can crash for some input. The function could for instance be the following one (choose your programming language to get a specialized example):

{{#tabs global="lang" }}
{{#tab name="C" }}
<!-- $MDX file=f.c -->
```c
int f(int x) {

  int arr[4] = {1, 2, 0, 4};

  if (x >= 0 && x < 4) {
    return 10 / arr[x];
  }

  return -1;
}
```
{{#endtab }}
{{#tab name="C++" }}
<!-- $MDX file=f.cpp -->
```cpp
extern "C" int f(int x) {

  int arr[4] = {1, 2, 0, 4};

  if (x >= 0 && x < 4) {
    return 10 / arr[x];
  }

  return -1;
}
```
{{#endtab }}
{{#tab name="Rust" }}
<!-- $MDX file=f.rs -->
```rs
#![no_main]

#[no_mangle]
pub extern "C" fn f(x: usize) -> i32 {

  let arr = [1, 2, 0, 4];

  if x < arr.len() {
    return 10 / arr[x];
  }

  -1
}
```
{{#endtab }}
{{#tab name="Zig" }}
<!-- $MDX file=f.zig -->
```rs
export fn f(x: usize) i32 {

  const arr = [_]i32{ 1, 2, 0, 4 };

  if (x < arr.len) {
    return @divTrunc(10, arr[x]);
  }

  return -1;
}
```
{{#endtab }}
{{#tab name="Wasm" }}
<!-- $MDX file=f.wat -->
```wasm
(module
  (memory 1)
  (func $f (export "f") (param $x i32) (result i32)
    (local $value i32)

    (i32.store (i32.mul (i32.const 4) (i32.const 0)) (i32.const 1))
    (i32.store (i32.mul (i32.const 4) (i32.const 1)) (i32.const 2))
    (i32.store (i32.mul (i32.const 4) (i32.const 2)) (i32.const 0))
    (i32.store (i32.mul (i32.const 4) (i32.const 3)) (i32.const 4))

    (if (i32.ge_u (local.get $x) (i32.const 4) )
      (then (return (i32.const -1))))

    (local.set $value
      (i32.load
        (i32.mul
          (local.get $x)
          (i32.const 4))))

    (i32.div_s
      (i32.const 10)
      (local.get $value))
  )
)
```
{{#endtab }}
{{#endtabs }}

We are going to use `owi` to look for a crash in the function. Owi has one subcommand for each programming language it supports. For instance, if you are analyzing a C program the command will be `owi c <...>`, whereas for a Rust program it will be `owi rust <...>`.

Then, we use the `--entry-point=f` option to tell Owi to starts its analysis on the function we are interested in.

Finally, we use the `--invoke-with-symbols` option to tell Owi it should invoke the functions with *symbolic* values. Here, it means that `x` will be a value representing "any possible integer", and not a concrete one. You'll learn more about this in the next example. What you should remember is that it allows Owi to check all possible execution path, for any value of `x`.

All the others parameters are only here to make the output deterministic while generating the documentation and you should ignore them.

{{#tabs global="lang" }}
{{#tab name="C" }}
```sh
$ owi c ./f.c --entry-point=f --invoke-with-symbols --no-assert-failure-expression-printing --verbosity=error
owi: [ERROR] Trap: integer divide by zero
model {
  symbol symbol_0 i32 2
}
owi: [ERROR] Reached problem!
[13]
```
{{#endtab }}
{{#tab name="C++" }}
```sh
$ owi c++ ./f.cpp --entry-point=f --invoke-with-symbols --no-assert-failure-expression-printing --verbosity=error
owi: [ERROR] Trap: integer divide by zero
model {
  symbol symbol_0 i32 2
}
owi: [ERROR] Reached problem!
[13]
```
{{#endtab }}
{{#tab name="Rust" }}
```sh
$ owi rust ./f.rs --entry-point=f --invoke-with-symbols --no-assert-failure-expression-printing --verbosity=error
owi: [ERROR] Trap: unreachable
model {
  symbol symbol_0 i32 2
}
owi: [ERROR] Reached problem!
[13]
```
{{#endtab }}
{{#tab name="Zig" }}
```sh
$ owi zig ./f.zig --entry-point=f --invoke-with-symbols --no-assert-failure-expression-printing --verbosity=error
owi: [ERROR] Trap: unreachable
model {
  symbol symbol_0 i32 2
}
owi: [ERROR] Reached problem!
[13]
```
{{#endtab }}
{{#tab name="Wasm" }}
```sh
$ owi sym ./f.wat --entry-point=f --invoke-with-symbols --no-assert-failure-expression-printing --verbosity=error
owi: [ERROR] Trap: integer divide by zero
model {
  symbol symbol_0 i32 2
}
owi: [ERROR] Reached problem!
[13]
```
{{#endtab }}
{{#endtabs }}

Owi says he reached a *trap*, which corresponds to a *programming error*. The exact trap depends on the input language and how it is compiled to Wasm. But here it'll either be "integer divide by zero" or "unreachable".

Then Owi gives us a *model*, that is, the set of input values of the program leading to this *trap*. The model is a list of *symbols*, each symbols representing an input.

Here we have a single symbol in the model, whose name is `symbol_0`, of type `i32` and whose value is `2`. And indeed, if we use `2` as the input value of the function `f`, there will be a crash in the program because of a division by zero!

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
  symbol symbol_0 i32 -1570748002
  symbol symbol_1 i32 -1425538774
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
  symbol symbol_0 i32 -1570748002
  symbol symbol_1 i32 -1425538774
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
  symbol symbol_0 i32 915013628
  symbol symbol_1 i32 -1225654275
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
  symbol symbol_0 i32 -722535840
  symbol symbol_1 i32 -418314849
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

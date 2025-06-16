# WebAssembly Quickstart

## Symbolic engine

The symbolic interpreter provides functions that allows to create *symbolic values*.
For instance, here, the `$i32_symbol` function creates a symbol of type `i32`.
This value will represent "any possible value".

Then, during the execution we collect informations and when an error is reached, we are able to find a concrete value for each symbol such that this value lead to the error.

For instance, in the following file, we define `x` as a symbolic variable.
Then if `5 < x`, we fail:

<!-- $MDX file=mini.wat -->
```wat
(module

  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start (local $x i32)
    (local.set $x (call $i32_symbol))

    (if (i32.lt_s (i32.const 5) (local.get $x)) (then
      unreachable
    ))
  )

  (start $start))
```


Let's see if owi is able to find a value for `x` that lead to an error:

```sh
$ owi sym ./mini.wat
owi: [ERROR] Trap: unreachable
model {
  symbol symbol_0 i32 6
}
owi: [ERROR] Reached problem!
[13]
```

Indeed, if `x` is equal to `6` then, the `unreachable` instruction will be reached.


## Concolic engine

The concolic engine works exactly in the same way than the symbolic engine:

```sh
$ owi conc ./mini.wat
owi: [ERROR] Trap: unreachable
Model:
 model {
  symbol symbol_1 i32 6
}
owi: [ERROR] Reached problem!
[13]
```

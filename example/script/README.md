# Using the `spectest` module

Given the following `print.wast` file:

<!-- $MDX file=print.wast -->
```wast
(module
  (func $print_i32 (import "spectest" "print_i32") (param i32))
  (func $main (call $print_i32 (i32.const 42)))
  (start $main)
)
```

You can print the value thanks to the `print_i32` function imported from the `spectest` module:

```sh
$ dune exec owi -- script ./print.wast
42
```

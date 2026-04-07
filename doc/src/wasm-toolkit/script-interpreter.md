## Script Interpreter using the `spectest` module

Given the following `print.wast` file:

<!-- $MDX file=print.wast -->
```wast
(module

  (func $print_i32 (import "spectest" "print_i32") (param i32))

  (func $main
    i32.const 42
    call $print_i32
  )

  (start $main)
)
```

You can print the value thanks to the `print_i32` function imported from the `spectest` module:

```sh
$ owi script ./print.wast
42
```

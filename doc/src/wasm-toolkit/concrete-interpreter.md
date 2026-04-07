## Concrete Interpreter

Given a file `42.wat` with the following content:

<!-- $MDX file=42.wat -->
```wat
(module $quickstart
  (func $f
    i32.const 20
    i32.const 22
    i32.add
    drop
  )
  (start $f)
)
```

Running the interpreter is as simple as:

```sh
$ owi run ./42.wat
```

Nothing is happening, so you can add the `-v` option to print an execution trace:

```sh
$ owi run ./42.wat -v
owi: [INFO] parsing      ...
owi: [INFO] checking     ...
owi: [INFO] checking     ...
owi: [INFO] typechecking ...
owi: [INFO] linking      ...
owi: [INFO] interpreting ...
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : call 0 (executed 0 times)
owi: [INFO] calling func  : func f
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : i32.const 20 (executed 0 times)
owi: [INFO] stack         : [ i32.const 20 ]
owi: [INFO] running instr : i32.const 22 (executed 0 times)
owi: [INFO] stack         : [ i32.const 22 ; i32.const 20 ]
owi: [INFO] running instr : i32.add (executed 0 times)
owi: [INFO] stack         : [ i32.const 42 ]
owi: [INFO] running instr : drop (executed 0 times)
```

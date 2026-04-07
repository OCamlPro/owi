# Converter

## Wasm2wat

Given a file `42.wasm`, you can convert it to `result.wat` and then run it:

```sh
$ owi wasm2wat 42.wasm -o result.wat
$ cat result.wat
(module
  (type (func))
  (func
    i32.const 20
    i32.const 22
    i32.add
    drop
  )
  (start 0)
)
$ owi run result.wat -v
owi: [INFO] parsing      ...
owi: [INFO] checking     ...
owi: [INFO] checking     ...
owi: [INFO] typechecking ...
owi: [INFO] linking      ...
owi: [INFO] interpreting ...
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : call 0 (executed 0 times)
owi: [INFO] calling func  : func anonymous
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : i32.const 20 (executed 0 times)
owi: [INFO] stack         : [ i32.const 20 ]
owi: [INFO] running instr : i32.const 22 (executed 0 times)
owi: [INFO] stack         : [ i32.const 22 ; i32.const 20 ]
owi: [INFO] running instr : i32.add (executed 0 times)
owi: [INFO] stack         : [ i32.const 42 ]
owi: [INFO] running instr : drop (executed 0 times)
```

## Wat2wasm

Given a file `42.wat`, you can convert it to `result.wasm` and then run it:

```sh
$ owi wat2wasm 42.wat -o result.wasm
$ owi run result.wasm -v
owi: [INFO] typechecking ...
owi: [INFO] linking      ...
owi: [INFO] interpreting ...
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : call 0 (executed 0 times)
owi: [INFO] calling func  : func anonymous
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : i32.const 20 (executed 0 times)
owi: [INFO] stack         : [ i32.const 20 ]
owi: [INFO] running instr : i32.const 22 (executed 0 times)
owi: [INFO] stack         : [ i32.const 22 ; i32.const 20 ]
owi: [INFO] running instr : i32.add (executed 0 times)
owi: [INFO] stack         : [ i32.const 42 ]
owi: [INFO] running instr : drop (executed 0 times)
```

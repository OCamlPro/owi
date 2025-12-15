# Comparing iso-functionnality of two modules

The `owi iso` command takes two Wasm modules as input.
Then, for every common exports between these two modules, Owi will check their equivalence.

Given the following `mul1.wat` file:

<!-- $MDX file=mul1.wat -->
```wat
(module

  (func (export "unused1") (param $x i32)
    local.get $x
    drop
  )

  (func (export "mul") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.mul
  )
)
```

And the following `mul2.wat` file:


<!-- $MDX file=mul2.wat -->
```wat
(module

  (func (export "unused2") (param $x i32) (param $y i64) (result i64)
    local.get $x
    (if (then (unreachable)))
    local.get $y
  )

  (func (export "mul") (param $x i32) (param $y i32) (result i32)
    local.get $y
    local.get $x
    i32.mul
    i32.const 1
    i32.add
  )
)
```

Owi can find an input for which the `mul` function of these two modules is not equivalent:

```sh
$ owi iso ./mul1.wat ./mul2.wat -v -w1
owi: [INFO] comparing ./mul1.wat and ./mul2.wat
owi: [INFO] module owi_iso_module1 is ./mul1.wat
owi: [INFO] module owi_iso_module2 is ./mul2.wat
owi: [INFO] Compiling ./mul1.wat
owi: [INFO] parsing      ...
owi: [INFO] checking     ...
owi: [INFO] typechecking ...
owi: [INFO] Compiling ./mul2.wat
owi: [INFO] parsing      ...
owi: [INFO] checking     ...
owi: [INFO] typechecking ...
owi: [INFO] common exports: mul
owi: [INFO] checking export mul
owi: [INFO] typechecking ...
owi: [INFO] linking      ...
owi: [INFO] typechecking ...
owi: [INFO] linking      ...
owi: [INFO] typechecking ...
owi: [INFO] linking      ...
owi: [INFO] interpreting ...
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : call 8 (executed 0 times)
owi: [INFO] calling func  : func start
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : call 3 (executed 0 times)
owi: [INFO] stack         : [ symbol_0 ]
owi: [INFO] running instr : call 3 (executed 0 times)
owi: [INFO] stack         : [ symbol_1 ; symbol_0 ]
owi: [INFO] running instr : call 7 (executed 0 times)
owi: [INFO] calling func  : func check_iso_func
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : local.get 0 (executed 0 times)
owi: [INFO] stack         : [ symbol_0 ]
owi: [INFO] running instr : local.get 1 (executed 0 times)
owi: [INFO] stack         : [ symbol_1 ; symbol_0 ]
owi: [INFO] running instr : call 0 (executed 0 times)
owi: [INFO] calling func  : func anonymous
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : local.get 0 (executed 0 times)
owi: [INFO] stack         : [ symbol_0 ]
owi: [INFO] running instr : local.get 1 (executed 0 times)
owi: [INFO] stack         : [ symbol_1 ; symbol_0 ]
owi: [INFO] running instr : i32.mul (executed 0 times)
owi: [INFO] stack         : [ (i32.mul symbol_0 symbol_1) ]
owi: [INFO] running instr : local.get 0 (executed 0 times)
owi: [INFO] stack         : [ symbol_0 ; (i32.mul symbol_0 symbol_1) ]
owi: [INFO] running instr : local.get 1 (executed 0 times)
owi: [INFO] stack         : [ symbol_1 ; symbol_0 ; (i32.mul symbol_0
                                                     symbol_1) ]
owi: [INFO] running instr : call 1 (executed 0 times)
owi: [INFO] calling func  : func anonymous
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : local.get 1 (executed 0 times)
owi: [INFO] stack         : [ symbol_1 ]
owi: [INFO] running instr : local.get 0 (executed 0 times)
owi: [INFO] stack         : [ symbol_0 ; symbol_1 ]
owi: [INFO] running instr : i32.mul (executed 0 times)
owi: [INFO] stack         : [ (i32.mul symbol_1 symbol_0) ]
owi: [INFO] running instr : i32.const 1 (executed 0 times)
owi: [INFO] stack         : [ 1 ; (i32.mul symbol_1 symbol_0) ]
owi: [INFO] running instr : i32.add (executed 0 times)
owi: [INFO] stack         : [ (i32.add (i32.mul symbol_1 symbol_0) 1) ;
            (i32.mul symbol_0 symbol_1) ]
owi: [INFO] running instr : i32.eq (executed 0 times)
owi: [INFO] stack         : [ (i32.of_bool
                               (bool.eq (i32.mul symbol_0 symbol_1)
                                (i32.add (i32.mul symbol_1 symbol_0) 1))) ]
owi: [INFO] running instr : call 2 (executed 0 times)
owi: [ERROR] Assert failure: (bool.eq (i32.mul symbol_0 symbol_1)
                              (i32.add (i32.mul symbol_1 symbol_0) 1))
model {
  symbol symbol_0 i32 0
  symbol symbol_1 i32 0
}
owi: [INFO] Completed paths: 1
owi: [ERROR] Reached problem!
[13]
```


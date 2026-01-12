# Replaying a model

Let's say you found a bug and want to check what is going on with the concrete input it contains.
The `replay` commands can help with that.

First, you need to perform a symbolic run and to store the output model in a file.
Given the following `mini.wat` file containing symbols:

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

You can get a model like this:

```sh
$ owi sym ./mini.wat > mini.scfg
owi: [ERROR] Trap: unreachable
owi: [ERROR] Reached problem!
[13]
```

Then you can replay the module execution with the values in the model like this:

```sh
$ owi replay --replay-file mini.scfg mini.wat -v
owi: [INFO] parsing      ...
owi: [INFO] checking     ...
owi: [INFO] typechecking ...
owi: [INFO] linking      ...
owi: [INFO] interpreting ...
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : call 1 (executed 0 times)
owi: [INFO] calling func  : func start
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : call 0 (executed 0 times)
owi: [INFO] stack         : [ i32.const 6 ]
owi: [INFO] running instr : local.set 0 (executed 0 times)
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : i32.const 5 (executed 0 times)
owi: [INFO] stack         : [ i32.const 5 ]
owi: [INFO] running instr : local.get 0 (executed 0 times)
owi: [INFO] stack         : [ i32.const 6 ; i32.const 5 ]
owi: [INFO] running instr : i32.lt_s (executed 0 times)
owi: [INFO] stack         : [ i32.const 1 ]
owi: [INFO] running instr : if (executed 0 times)
owi: [INFO] stack         : [  ]
owi: [INFO] running instr : unreachable (executed 0 times)
owi: [ERROR] unreachable
[96]
```


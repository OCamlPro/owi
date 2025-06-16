  $ owi instrument plus.wat
  $ cat plus.instrumented.wat
  (module
    (import "owi" "assert" (func $assert  (param i32)))
    (type (func (param $x i32) (result i32)))
    (type (func))
    (type (func (param i32)))
    (type (func (result i32)))
    (func $plus_three (param $x i32) (result i32)
      i32.const 3
      local.get 0
      i32.add
    )
    (func $start
      i32.const 42
      call 3
      drop
    )
    (func $__weasel_plus_three (param $x i32) (result i32) (local $__weasel_temp i32) (local $__weasel_res_0 i32)
      local.get 0
      call 1
      local.set 2
      local.get 2
      local.get 0
      i32.const 3
      i32.add
      i32.eq
      call 0
      local.get 2
    )
    (start 2)
  )
  $ owi instrument plus.wat --symbolic
  $ cat plus.instrumented.wat
  (module
    (import "owi" "assert" (func $assert  (param i32)))
    (type (func (param $x i32) (result i32)))
    (type (func))
    (type (func (param i32)))
    (type (func (result i32)))
    (func $plus_three (param $x i32) (result i32)
      i32.const 3
      local.get 0
      i32.add
    )
    (func $start
      i32.const 42
      call 3
      drop
    )
    (func $__weasel_plus_three (param $x i32) (result i32) (local $__weasel_temp i32) (local $__weasel_res_0 i32)
      local.get 0
      call 1
      local.set 2
      local.get 2
      local.get 0
      i32.const 3
      i32.add
      i32.eq
      call 0
      local.get 2
    )
    (start 2)
  )
  $ owi sym plus.wat --rac -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 2
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ 42 ]
  owi: [INFO] running instr : call 3
  owi: [INFO] calling func  : func __weasel_plus_three
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func plus_three
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 3
  owi: [INFO] stack         : [ 3 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ; 3 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 45 ]
  owi: [INFO] running instr : local.set 2
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ 45 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ; 45 ]
  owi: [INFO] running instr : i32.const 3
  owi: [INFO] stack         : [ 3 ; 42 ; 45 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 45 ; 45 ]
  owi: [INFO] running instr : i32.eq
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ 45 ]
  owi: [INFO] running instr : drop
  owi: [INFO] Completed paths: 1
  All OK!
  $ owi sym plus.wat --srac -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 2
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ 42 ]
  owi: [INFO] running instr : call 3
  owi: [INFO] calling func  : func __weasel_plus_three
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func plus_three
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 3
  owi: [INFO] stack         : [ 3 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ; 3 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 45 ]
  owi: [INFO] running instr : local.set 2
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ 45 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ; 45 ]
  owi: [INFO] running instr : i32.const 3
  owi: [INFO] stack         : [ 3 ; 42 ; 45 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 45 ; 45 ]
  owi: [INFO] running instr : i32.eq
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ 45 ]
  owi: [INFO] running instr : drop
  owi: [INFO] Completed paths: 1
  All OK!

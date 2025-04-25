  $ owi instrument sum.wat
  $ cat sum.instrumented.wat
  (module
    (import "symbolic" "assert" (func $assert  (param i32)))
    (type (sub final  (func (param $p1 i32) (param $p2 i32) (param $p3 i32) (param $p4 i32) (result i32))))
    (type (sub final  (func)))
    (type (sub final  (func (param i32))))
    (type (sub final  (func (result i32))))
    (type (sub final  (func (param i32) (result i32))))
    (func $sum (param $p1 i32) (param $p2 i32) (param $p3 i32) (param $p4 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.add
      i32.add
      i32.add
    )
    (func $start
      i32.const 42
      i32.const 42
      i32.const 42
      i32.const 42
      call 3
      drop
    )
    (func $__weasel_sum (param $p1 i32) (param $p2 i32) (param $p3 i32) (param $p4 i32) (result i32) (local $__weasel_temp i32) (local $__weasel_res_0 i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call 1
      local.set 5
      local.get 5
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.add
      i32.add
      i32.add
      i32.eq
      call 0
      local.get 5
    )
    (start 2)
  )
  $ owi instrument sum.wat --symbolic
  $ cat sum.instrumented.wat
  (module
    (import "symbolic" "assert" (func $assert  (param i32)))
    (type (sub final  (func (param $p1 i32) (param $p2 i32) (param $p3 i32) (param $p4 i32) (result i32))))
    (type (sub final  (func)))
    (type (sub final  (func (param i32))))
    (type (sub final  (func (result i32))))
    (type (sub final  (func (param i32) (result i32))))
    (func $sum (param $p1 i32) (param $p2 i32) (param $p3 i32) (param $p4 i32) (result i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.add
      i32.add
      i32.add
    )
    (func $start
      i32.const 42
      i32.const 42
      i32.const 42
      i32.const 42
      call 3
      drop
    )
    (func $__weasel_sum (param $p1 i32) (param $p2 i32) (param $p3 i32) (param $p4 i32) (result i32) (local $__weasel_temp i32) (local $__weasel_res_0 i32)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      call 1
      local.set 5
      local.get 5
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.add
      i32.add
      i32.add
      i32.eq
      call 0
      local.get 5
    )
    (start 2)
  )
  $ owi sym sum.wat --rac -v
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
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ 42 ; 42 ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ; 42 ]
  owi: [INFO] running instr : call 3
  owi: [INFO] calling func  : func __weasel_sum
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 42 ; 42 ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ]
  owi: [INFO] running instr : local.get 3
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ; 42 ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func sum
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 42 ; 42 ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ]
  owi: [INFO] running instr : local.get 3
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ; 42 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 84 ; 42 ; 42 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 126 ; 42 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 168 ]
  owi: [INFO] running instr : local.set 5
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 5
  owi: [INFO] stack         : [ 168 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ; 168 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 42 ; 42 ; 168 ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ; 168 ]
  owi: [INFO] running instr : local.get 3
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ; 42 ; 168 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 84 ; 42 ; 42 ; 168 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 126 ; 42 ; 168 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 168 ; 168 ]
  owi: [INFO] running instr : i32.eq
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 5
  owi: [INFO] stack         : [ 168 ]
  owi: [INFO] running instr : drop
  owi: [INFO] Completed paths: 1
  All OK!
  $ owi sym sum.wat --srac -v
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
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ 42 ; 42 ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ; 42 ]
  owi: [INFO] running instr : call 3
  owi: [INFO] calling func  : func __weasel_sum
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 42 ; 42 ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ]
  owi: [INFO] running instr : local.get 3
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ; 42 ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func sum
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 42 ; 42 ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ]
  owi: [INFO] running instr : local.get 3
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ; 42 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 84 ; 42 ; 42 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 126 ; 42 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 168 ]
  owi: [INFO] running instr : local.set 5
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 5
  owi: [INFO] stack         : [ 168 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 42 ; 168 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 42 ; 42 ; 168 ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ; 168 ]
  owi: [INFO] running instr : local.get 3
  owi: [INFO] stack         : [ 42 ; 42 ; 42 ; 42 ; 168 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 84 ; 42 ; 42 ; 168 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 126 ; 42 ; 168 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 168 ; 168 ]
  owi: [INFO] running instr : i32.eq
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 5
  owi: [INFO] stack         : [ 168 ]
  owi: [INFO] running instr : drop
  owi: [INFO] Completed paths: 1
  All OK!

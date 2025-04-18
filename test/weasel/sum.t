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
  $ owi sym sum.wat --rac --debug
  parsing      ...
  Contract of function 0
  Preconditions:
    
  Postconditions:
    (= result (+ $p1 (+ $p2 (+ $p3 $p4))))
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  typechecking ...
  linking      ...
  interpreting ...
  stack         : [  ]
  running instr : call 2
  calling func  : func start
  stack         : [  ]
  running instr : i32.const 42
  stack         : [ 42 ]
  running instr : i32.const 42
  stack         : [ 42 ; 42 ]
  running instr : i32.const 42
  stack         : [ 42 ; 42 ; 42 ]
  running instr : i32.const 42
  stack         : [ 42 ; 42 ; 42 ; 42 ]
  running instr : call 3
  calling func  : func __weasel_sum
  stack         : [  ]
  running instr : local.get 0
  stack         : [ 42 ]
  running instr : local.get 1
  stack         : [ 42 ; 42 ]
  running instr : local.get 2
  stack         : [ 42 ; 42 ; 42 ]
  running instr : local.get 3
  stack         : [ 42 ; 42 ; 42 ; 42 ]
  running instr : call 1
  calling func  : func sum
  stack         : [  ]
  running instr : local.get 0
  stack         : [ 42 ]
  running instr : local.get 1
  stack         : [ 42 ; 42 ]
  running instr : local.get 2
  stack         : [ 42 ; 42 ; 42 ]
  running instr : local.get 3
  stack         : [ 42 ; 42 ; 42 ; 42 ]
  running instr : i32.add
  stack         : [ 84 ; 42 ; 42 ]
  running instr : i32.add
  stack         : [ 126 ; 42 ]
  running instr : i32.add
  stack         : [ 168 ]
  stack         : [ 168 ]
  running instr : local.set 5
  stack         : [  ]
  running instr : local.get 5
  stack         : [ 168 ]
  running instr : local.get 0
  stack         : [ 42 ; 168 ]
  running instr : local.get 1
  stack         : [ 42 ; 42 ; 168 ]
  running instr : local.get 2
  stack         : [ 42 ; 42 ; 42 ; 168 ]
  running instr : local.get 3
  stack         : [ 42 ; 42 ; 42 ; 42 ; 168 ]
  running instr : i32.add
  stack         : [ 84 ; 42 ; 42 ; 168 ]
  running instr : i32.add
  stack         : [ 126 ; 42 ; 168 ]
  running instr : i32.add
  stack         : [ 168 ; 168 ]
  running instr : i32.eq
  stack         : [ 1 ]
  running instr : call 0
  stack         : [  ]
  running instr : local.get 5
  stack         : [ 168 ]
  stack         : [ 168 ]
  running instr : drop
  stack         : [  ]
  stack         : [  ]
  All OK
  $ owi sym sum.wat --srac --debug
  parsing      ...
  Contract of function 0
  Preconditions:
    
  Postconditions:
    (= result (+ $p1 (+ $p2 (+ $p3 $p4))))
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  typechecking ...
  linking      ...
  interpreting ...
  stack         : [  ]
  running instr : call 2
  calling func  : func start
  stack         : [  ]
  running instr : i32.const 42
  stack         : [ 42 ]
  running instr : i32.const 42
  stack         : [ 42 ; 42 ]
  running instr : i32.const 42
  stack         : [ 42 ; 42 ; 42 ]
  running instr : i32.const 42
  stack         : [ 42 ; 42 ; 42 ; 42 ]
  running instr : call 3
  calling func  : func __weasel_sum
  stack         : [  ]
  running instr : local.get 0
  stack         : [ 42 ]
  running instr : local.get 1
  stack         : [ 42 ; 42 ]
  running instr : local.get 2
  stack         : [ 42 ; 42 ; 42 ]
  running instr : local.get 3
  stack         : [ 42 ; 42 ; 42 ; 42 ]
  running instr : call 1
  calling func  : func sum
  stack         : [  ]
  running instr : local.get 0
  stack         : [ 42 ]
  running instr : local.get 1
  stack         : [ 42 ; 42 ]
  running instr : local.get 2
  stack         : [ 42 ; 42 ; 42 ]
  running instr : local.get 3
  stack         : [ 42 ; 42 ; 42 ; 42 ]
  running instr : i32.add
  stack         : [ 84 ; 42 ; 42 ]
  running instr : i32.add
  stack         : [ 126 ; 42 ]
  running instr : i32.add
  stack         : [ 168 ]
  stack         : [ 168 ]
  running instr : local.set 5
  stack         : [  ]
  running instr : local.get 5
  stack         : [ 168 ]
  running instr : local.get 0
  stack         : [ 42 ; 168 ]
  running instr : local.get 1
  stack         : [ 42 ; 42 ; 168 ]
  running instr : local.get 2
  stack         : [ 42 ; 42 ; 42 ; 168 ]
  running instr : local.get 3
  stack         : [ 42 ; 42 ; 42 ; 42 ; 168 ]
  running instr : i32.add
  stack         : [ 84 ; 42 ; 42 ; 168 ]
  running instr : i32.add
  stack         : [ 126 ; 42 ; 168 ]
  running instr : i32.add
  stack         : [ 168 ; 168 ]
  running instr : i32.eq
  stack         : [ 1 ]
  running instr : call 0
  stack         : [  ]
  running instr : local.get 5
  stack         : [ 168 ]
  stack         : [ 168 ]
  running instr : drop
  stack         : [  ]
  stack         : [  ]
  All OK

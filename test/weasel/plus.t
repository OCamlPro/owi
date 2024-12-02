  $ owi instrument plus.wat
  $ cat plus.instrumented.wat
  (module
    (import "symbolic" "assert" (func $assert  (param i32)))
    (type (sub final  (func (param $x i32) (result i32))))
    (type (sub final  (func)))
    (type (sub final  (func (param i32))))
    (type (sub final  (func (result i32))))
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
    (import "symbolic" "assert" (func $assert  (param i32)))
    (type (sub final  (func (param $x i32) (result i32))))
    (type (sub final  (func)))
    (type (sub final  (func (param i32))))
    (type (sub final  (func (result i32))))
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
  $ owi sym plus.wat --rac --debug
  parsing      ...
  Contract of function $plus_three
  Preconditions:
    
  Postconditions:
    (= result (+ $x (i32 3)))
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  typechecking ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 2
  calling func : func start
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ 42 ]
  running instr: call 3
  calling func : func __weasel_plus_three
  stack        : [  ]
  running instr: local.get 0
  stack        : [ 42 ]
  running instr: call 1
  calling func : func plus_three
  stack        : [  ]
  running instr: i32.const 3
  stack        : [ 3 ]
  running instr: local.get 0
  stack        : [ 42 ; 3 ]
  running instr: i32.add
  stack        : [ 45 ]
  stack        : [ 45 ]
  running instr: local.set 2
  stack        : [  ]
  running instr: local.get 2
  stack        : [ 45 ]
  running instr: local.get 0
  stack        : [ 42 ; 45 ]
  running instr: i32.const 3
  stack        : [ 3 ; 42 ; 45 ]
  running instr: i32.add
  stack        : [ 45 ; 45 ]
  running instr: i32.eq
  stack        : [ 1 ]
  running instr: call 0
  stack        : [  ]
  running instr: local.get 2
  stack        : [ 45 ]
  stack        : [ 45 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  All OK
  $ owi sym plus.wat --srac --debug
  parsing      ...
  Contract of function $plus_three
  Preconditions:
    
  Postconditions:
    (= result (+ $x (i32 3)))
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  typechecking ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 2
  calling func : func start
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ 42 ]
  running instr: call 3
  calling func : func __weasel_plus_three
  stack        : [  ]
  running instr: local.get 0
  stack        : [ 42 ]
  running instr: call 1
  calling func : func plus_three
  stack        : [  ]
  running instr: i32.const 3
  stack        : [ 3 ]
  running instr: local.get 0
  stack        : [ 42 ; 3 ]
  running instr: i32.add
  stack        : [ 45 ]
  stack        : [ 45 ]
  running instr: local.set 2
  stack        : [  ]
  running instr: local.get 2
  stack        : [ 45 ]
  running instr: local.get 0
  stack        : [ 42 ; 45 ]
  running instr: i32.const 3
  stack        : [ 3 ; 42 ; 45 ]
  running instr: i32.add
  stack        : [ 45 ; 45 ]
  running instr: i32.eq
  stack        : [ 1 ]
  running instr: call 0
  stack        : [  ]
  running instr: local.get 2
  stack        : [ 45 ]
  stack        : [ 45 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  All OK

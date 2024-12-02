  $ owi instrument forall.wat
  $ cat forall.instrumented.wat
  (module
    (import "symbolic" "assert" (func $assert  (param i32)))
    (type (sub final  (func)))
    (type (sub final  (func (param i32))))
    (type (sub final  (func (result i32))))
    (type (sub final  (func (param i32) (result i32))))
    (func $start
      
    )
    (func $__weasel_start (local $__weasel_temp i32) (local $__weasel_binder_0 i32)
      (block $__weasel_forall (result i32)
        i32.const 1
        local.set 1
        i32.const 1
        (loop $__weasel_loop (param i32) (result i32)
          local.get 1
          i32.const 100
          i32.le_s
          i32.and
          local.tee 0
          local.get 0
          i32.const 1
          i32.xor
          br_if 1
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 1
          i32.const 10
          i32.le_s
          br_if 0))
      call 0
      call 1
    )
    (start 2)
  )
  $ owi instrument forall.wat --symbolic
  $ cat forall.instrumented.wat
  (module
    (import "symbolic" "assert" (func $assert  (param i32)))
    (type (sub final  (func)))
    (type (sub final  (func (param i32))))
    (type (sub final  (func (result i32))))
    (type (sub final  (func (param i32) (result i32))))
    (func $start
      
    )
    (func $__weasel_start (local $__weasel_temp i32) (local $__weasel_binder_0 i32)
      (block $__weasel_forall (result i32)
        i32.const 1
        local.set 1
        i32.const 1
        (loop $__weasel_loop (param i32) (result i32)
          local.get 1
          i32.const 100
          i32.le_s
          i32.and
          local.tee 0
          local.get 0
          i32.const 1
          i32.xor
          br_if 1
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 1
          i32.const 10
          i32.le_s
          br_if 0))
      call 0
      call 1
    )
    (start 2)
  )
  $ owi sym forall.wat --rac --debug
  parsing      ...
  Contract of function $start
  Preconditions:
    (∀ $x:i32
      (⇒ (∧ (≥ $x (i32 1)) (≤ $x (i32 10))) (≤ $x (i32 100))))
  Postconditions:
    
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
  calling func : func __weasel_start
  stack        : [  ]
  running instr: (block $__weasel_forall (result i32)
    i32.const 1
    local.set 1
    i32.const 1
    (loop $__weasel_loop (param i32) (result i32)
      local.get 1
      i32.const 100
      i32.le_s
      i32.and
      local.tee 0
      local.get 0
      i32.const 1
      i32.xor
      br_if 1
      local.get 1
      i32.const 1
      i32.add
      local.set 1
      local.get 1
      i32.const 10
      i32.le_s
      br_if 0))
  stack        : [  ]
  running instr: i32.const 1
  stack        : [ 1 ]
  running instr: local.set 1
  stack        : [  ]
  running instr: i32.const 1
  stack        : [ 1 ]
  running instr: (loop $__weasel_loop (param i32) (result i32)
    local.get 1
    i32.const 100
    i32.le_s
    i32.and
    local.tee 0
    local.get 0
    i32.const 1
    i32.xor
    br_if 1
    local.get 1
    i32.const 1
    i32.add
    local.set 1
    local.get 1
    i32.const 10
    i32.le_s
    br_if 0)
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 1 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 1 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.add
  stack        : [ 2 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 2 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 2 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 2 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 2 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 2 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 2 ; 1 ]
  running instr: i32.add
  stack        : [ 3 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 3 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 3 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 3 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 3 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 3 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 3 ; 1 ]
  running instr: i32.add
  stack        : [ 4 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 4 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 4 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 4 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 4 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 4 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 4 ; 1 ]
  running instr: i32.add
  stack        : [ 5 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 5 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 5 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 5 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 5 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 5 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 5 ; 1 ]
  running instr: i32.add
  stack        : [ 6 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 6 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 6 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 6 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 6 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 6 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 6 ; 1 ]
  running instr: i32.add
  stack        : [ 7 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 7 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 7 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 7 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 7 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 7 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 7 ; 1 ]
  running instr: i32.add
  stack        : [ 8 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 8 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 8 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 8 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 8 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 8 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 8 ; 1 ]
  running instr: i32.add
  stack        : [ 9 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 9 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 9 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 9 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 9 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 9 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 9 ; 1 ]
  running instr: i32.add
  stack        : [ 10 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 10 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 10 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 10 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 10 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 10 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 10 ; 1 ]
  running instr: i32.add
  stack        : [ 11 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 11 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 11 ; 1 ]
  running instr: i32.le_s
  stack        : [ 0 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  stack        : [ 1 ]
  stack        : [ 1 ]
  running instr: call 0
  stack        : [  ]
  running instr: call 1
  calling func : func start
  stack        : [  ]
  stack        : [  ]
  stack        : [  ]
  All OK
  $ owi sym forall.wat --srac --debug
  parsing      ...
  Contract of function $start
  Preconditions:
    (∀ $x:i32
      (⇒ (∧ (≥ $x (i32 1)) (≤ $x (i32 10))) (≤ $x (i32 100))))
  Postconditions:
    
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
  calling func : func __weasel_start
  stack        : [  ]
  running instr: (block $__weasel_forall (result i32)
    i32.const 1
    local.set 1
    i32.const 1
    (loop $__weasel_loop (param i32) (result i32)
      local.get 1
      i32.const 100
      i32.le_s
      i32.and
      local.tee 0
      local.get 0
      i32.const 1
      i32.xor
      br_if 1
      local.get 1
      i32.const 1
      i32.add
      local.set 1
      local.get 1
      i32.const 10
      i32.le_s
      br_if 0))
  stack        : [  ]
  running instr: i32.const 1
  stack        : [ 1 ]
  running instr: local.set 1
  stack        : [  ]
  running instr: i32.const 1
  stack        : [ 1 ]
  running instr: (loop $__weasel_loop (param i32) (result i32)
    local.get 1
    i32.const 100
    i32.le_s
    i32.and
    local.tee 0
    local.get 0
    i32.const 1
    i32.xor
    br_if 1
    local.get 1
    i32.const 1
    i32.add
    local.set 1
    local.get 1
    i32.const 10
    i32.le_s
    br_if 0)
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 1 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 1 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.add
  stack        : [ 2 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 2 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 2 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 2 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 2 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 2 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 2 ; 1 ]
  running instr: i32.add
  stack        : [ 3 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 3 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 3 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 3 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 3 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 3 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 3 ; 1 ]
  running instr: i32.add
  stack        : [ 4 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 4 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 4 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 4 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 4 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 4 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 4 ; 1 ]
  running instr: i32.add
  stack        : [ 5 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 5 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 5 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 5 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 5 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 5 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 5 ; 1 ]
  running instr: i32.add
  stack        : [ 6 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 6 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 6 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 6 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 6 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 6 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 6 ; 1 ]
  running instr: i32.add
  stack        : [ 7 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 7 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 7 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 7 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 7 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 7 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 7 ; 1 ]
  running instr: i32.add
  stack        : [ 8 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 8 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 8 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 8 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 8 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 8 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 8 ; 1 ]
  running instr: i32.add
  stack        : [ 9 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 9 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 9 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 9 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 9 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 9 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 9 ; 1 ]
  running instr: i32.add
  stack        : [ 10 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 10 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 10 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 10 ; 1 ]
  running instr: i32.const 100
  stack        : [ 100 ; 10 ; 1 ]
  running instr: i32.le_s
  stack        : [ 1 ; 1 ]
  running instr: i32.and
  stack        : [ 1 ]
  running instr: local.tee 0
  stack        : [ 1 ]
  running instr: local.get 0
  stack        : [ 1 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 1 ; 1 ]
  running instr: i32.xor
  stack        : [ 0 ; 1 ]
  running instr: br_if 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 10 ; 1 ]
  running instr: i32.const 1
  stack        : [ 1 ; 10 ; 1 ]
  running instr: i32.add
  stack        : [ 11 ; 1 ]
  running instr: local.set 1
  stack        : [ 1 ]
  running instr: local.get 1
  stack        : [ 11 ; 1 ]
  running instr: i32.const 10
  stack        : [ 10 ; 11 ; 1 ]
  running instr: i32.le_s
  stack        : [ 0 ; 1 ]
  running instr: br_if 0
  stack        : [ 1 ]
  stack        : [ 1 ]
  stack        : [ 1 ]
  running instr: call 0
  stack        : [  ]
  running instr: call 1
  calling func : func start
  stack        : [  ]
  stack        : [  ]
  stack        : [  ]
  All OK

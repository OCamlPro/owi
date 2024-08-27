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
    (import "symbolic" "i32_symbol" (func $i32_symbol  (result i32)))
    (import "symbolic" "i64_symbol" (func $i64_symbol  (result i64)))
    (import "symbolic" "f32_symbol" (func $f32_symbol  (result f32)))
    (import "symbolic" "f64_symbol" (func $f64_symbol  (result f64)))
    (import "symbolic" "assume" (func $assume  (param i32)))
    (import "symbolic" "assert" (func $assert  (param i32)))
    (type (sub final  (func)))
    (type (sub final  (func (result i32))))
    (type (sub final  (func (result i64))))
    (type (sub final  (func (result f32))))
    (type (sub final  (func (result f64))))
    (type (sub final  (func (param i32))))
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
      call 5
      call 6
    )
    (start 7)
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
  stack        : [ (i32 1) ]
  running instr: local.set 1
  stack        : [  ]
  running instr: i32.const 1
  stack        : [ (i32 1) ]
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
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 1) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 2) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 2) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 2) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 2) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 2) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 2) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 2) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 3) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 3) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 3) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 3) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 3) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 3) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 3) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 4) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 4) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 4) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 4) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 4) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 4) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 4) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 5) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 5) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 5) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 5) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 5) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 5) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 5) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 6) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 6) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 6) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 6) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 6) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 6) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 6) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 7) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 7) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 7) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 7) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 7) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 7) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 7) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 8) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 8) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 8) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 8) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 8) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 8) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 8) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 9) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 9) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 9) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 9) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 9) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 9) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 9) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 10) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 10) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 10) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 10) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 10) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 10) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 10) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 11) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 11) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 11) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  stack        : [ (i32 1) ]
  stack        : [ (i32 1) ]
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
  running instr: call 7
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
  stack        : [ (i32 1) ]
  running instr: local.set 1
  stack        : [  ]
  running instr: i32.const 1
  stack        : [ (i32 1) ]
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
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 1) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 2) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 2) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 2) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 2) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 2) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 2) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 2) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 3) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 3) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 3) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 3) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 3) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 3) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 3) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 4) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 4) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 4) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 4) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 4) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 4) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 4) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 5) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 5) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 5) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 5) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 5) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 5) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 5) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 6) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 6) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 6) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 6) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 6) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 6) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 6) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 7) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 7) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 7) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 7) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 7) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 7) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 7) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 8) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 8) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 8) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 8) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 8) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 8) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 8) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 9) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 9) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 9) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 9) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 9) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 9) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 9) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 10) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 10) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 10) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 10) ; (i32 1) ]
  running instr: i32.const 100
  stack        : [ (i32 100) ; (i32 10) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.and
  stack        : [ (i32 1) ]
  running instr: local.tee 0
  stack        : [ (i32 1) ]
  running instr: local.get 0
  stack        : [ (i32 1) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 1) ; (i32 1) ]
  running instr: i32.xor
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 10) ; (i32 1) ]
  running instr: i32.const 1
  stack        : [ (i32 1) ; (i32 10) ; (i32 1) ]
  running instr: i32.add
  stack        : [ (i32 11) ; (i32 1) ]
  running instr: local.set 1
  stack        : [ (i32 1) ]
  running instr: local.get 1
  stack        : [ (i32 11) ; (i32 1) ]
  running instr: i32.const 10
  stack        : [ (i32 10) ; (i32 11) ; (i32 1) ]
  running instr: i32.le_s
  stack        : [ (i32 0) ; (i32 1) ]
  running instr: br_if 0
  stack        : [ (i32 1) ]
  stack        : [ (i32 1) ]
  stack        : [ (i32 1) ]
  running instr: call 5
  stack        : [  ]
  running instr: call 6
  calling func : func start
  stack        : [  ]
  stack        : [  ]
  stack        : [  ]
  All OK

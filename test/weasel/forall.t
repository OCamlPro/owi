  $ owi instrument forall.wat
  $ owi sym forall.instrumented.wat --debug
  parsing      ...
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
  $ owi sym forall.instrumented.wasm --debug
  typechecking ...
  typechecking ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 7
  calling func : func anonymous
  stack        : [  ]
  running instr: (block (result i32)
    i32.const 1
    local.set 1
    i32.const 1
    (loop (param i32) (result i32)
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
  running instr: (loop (param i32) (result i32)
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
  calling func : func anonymous
  stack        : [  ]
  stack        : [  ]
  stack        : [  ]
  All OK
  $ owi wasm2wat forall.instrumented.wasm > forall.instrumented2.wat
  $ owi sym forall.instrumented2.wat --debug
  parsing      ...
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
  calling func : func anonymous
  stack        : [  ]
  running instr: (block (result i32)
    i32.const 1
    local.set 1
    i32.const 1
    (loop (param i32) (result i32)
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
  running instr: (loop (param i32) (result i32)
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
  calling func : func anonymous
  stack        : [  ]
  stack        : [  ]
  stack        : [  ]
  All OK

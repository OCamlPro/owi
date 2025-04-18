  $ owi c alloc.c -o alloc.wasm -O0 --no-value
  Assert failure: false
  model {
    symbol symbol_0 i32
  }
  Reached problem!
  [13]

  $ owi replay --replay-file alloc.scfg alloc.wasm --entry-point=main --debug
  typechecking ...
  typechecking ...
  linking      ...
  interpreting ...
  stack         : [  ]
  running instr : i32.const 1024
  stack         : [ i32.const 1024 ]
  running instr : i32.const 0
  stack         : [ i32.const 0 ; i32.const 1024 ]
  running instr : i32.const 4
  stack         : [ i32.const 4 ; i32.const 0 ; i32.const 1024 ]
  running instr : memory.init 0
  stack         : [  ]
  running instr : data.drop 0
  stack         : [  ]
  stack         : [  ]
  running instr : call 12
  calling func  : func anonymous
  stack         : [  ]
  running instr : i32.const 0
  stack         : [ i32.const 0 ]
  running instr : i32.const 0
  stack         : [ i32.const 0 ; i32.const 0 ]
  running instr : call 8
  calling func  : func anonymous
  stack         : [  ]
  running instr : call 7
  calling func  : func anonymous
  stack         : [  ]
  running instr : global.get 0
  stack         : [ i32.const 8389648 ]
  running instr : i32.const 16
  stack         : [ i32.const 16 ; i32.const 8389648 ]
  running instr : i32.sub
  stack         : [ i32.const 8389632 ]
  running instr : local.tee 0
  stack         : [ i32.const 8389632 ]
  running instr : global.set 0
  stack         : [  ]
  running instr : local.get 0
  stack         : [ i32.const 8389632 ]
  running instr : call 3
  stack         : [ i32.const 9 ; i32.const 8389632 ]
  running instr : i32.store offset=12 align=4
  stack         : [  ]
  running instr : local.get 0
  stack         : [ i32.const 8389632 ]
  running instr : i32.load offset=12 align=4
  stack         : [ i32.const 9 ]
  running instr : i32.const 10
  stack         : [ i32.const 10 ; i32.const 9 ]
  running instr : i32.lt_s
  stack         : [ i32.const 1 ]
  running instr : i32.const 1
  stack         : [ i32.const 1 ; i32.const 1 ]
  running instr : i32.and
  stack         : [ i32.const 1 ]
  running instr : call 4
  stack         : [  ]
  running instr : local.get 0
  stack         : [ i32.const 8389632 ]
  running instr : i32.load offset=12 align=4
  stack         : [ i32.const 9 ]
  running instr : i32.const 0
  stack         : [ i32.const 0 ; i32.const 9 ]
  running instr : i32.gt_s
  stack         : [ i32.const 1 ]
  running instr : i32.const 1
  stack         : [ i32.const 1 ; i32.const 1 ]
  running instr : i32.and
  stack         : [ i32.const 1 ]
  running instr : call 4
  stack         : [  ]
  running instr : local.get 0
  stack         : [ i32.const 8389632 ]
  running instr : local.get 0
  stack         : [ i32.const 8389632 ; i32.const 8389632 ]
  running instr : i32.load offset=12 align=4
  stack         : [ i32.const 9 ; i32.const 8389632 ]
  running instr : call 5
  calling func  : func anonymous
  stack         : [  ]
  running instr : block
  stack         : [  ]
  running instr : local.get 0
  stack         : [ i32.const 9 ]
  running instr : br_if 0
  stack         : [  ]
  running instr : i32.const 0
  stack         : [ i32.const 0 ]
  running instr : i32.const 1
  stack         : [ i32.const 1 ; i32.const 0 ]
  running instr : local.get 0
  stack         : [ i32.const 9 ; i32.const 1 ; i32.const 0 ]
  running instr : i32.clz
  stack         : [ i32.const 28 ; i32.const 1 ; i32.const 0 ]
  running instr : i32.const 31
  stack         : [ i32.const 31 ; i32.const 28 ; i32.const 1 ; i32.const 0 ]
  running instr : i32.xor
  stack         : [ i32.const 3 ; i32.const 1 ; i32.const 0 ]
  running instr : local.tee 1
  stack         : [ i32.const 3 ; i32.const 1 ; i32.const 0 ]
  running instr : i32.shl
  stack         : [ i32.const 8 ; i32.const 0 ]
  running instr : i32.const 16
  stack         : [ i32.const 16 ; i32.const 8 ; i32.const 0 ]
  running instr : local.get 1
  stack         : [ i32.const 3 ; i32.const 16 ; i32.const 8 ; i32.const 0 ]
  running instr : i32.const 5
  stack         : [ i32.const 5 ; i32.const 3 ; i32.const 16 ; i32.const 8 ; i32.const 0 ]
  running instr : i32.lt_u
  stack         : [ i32.const 1 ; i32.const 16 ; i32.const 8 ; i32.const 0 ]
  running instr : select
  stack         : [ i32.const 8 ; i32.const 0 ]
  running instr : local.tee 1
  stack         : [ i32.const 8 ; i32.const 0 ]
  running instr : i32.const 0
  stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  running instr : i32.load offset=1024 align=4
  stack         : [ i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  running instr : local.tee 2
  stack         : [ i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  running instr : local.get 1
  stack         : [ i32.const 8 ; i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  running instr : i32.const -1
  stack         : [ i32.const -1 ; i32.const 8 ; i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  running instr : i32.add
  stack         : [ i32.const 7 ; i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  running instr : i32.and
  stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  running instr : local.tee 1
  stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  running instr : i32.sub
  stack         : [ i32.const 8 ; i32.const 0 ]
  running instr : i32.const 0
  stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  running instr : local.get 1
  stack         : [ i32.const 0 ; i32.const 0 ; i32.const 8 ; i32.const 0 ]
  running instr : select
  stack         : [ i32.const 0 ; i32.const 0 ]
  running instr : local.get 2
  stack         : [ i32.const 8389648 ; i32.const 0 ; i32.const 0 ]
  running instr : i32.add
  stack         : [ i32.const 8389648 ; i32.const 0 ]
  running instr : local.tee 1
  stack         : [ i32.const 8389648 ; i32.const 0 ]
  running instr : local.get 0
  stack         : [ i32.const 9 ; i32.const 8389648 ; i32.const 0 ]
  running instr : i32.add
  stack         : [ i32.const 8389657 ; i32.const 0 ]
  running instr : i32.store offset=1024 align=4
  stack         : [  ]
  running instr : local.get 1
  stack         : [ i32.const 8389648 ]
  running instr : local.get 0
  stack         : [ i32.const 9 ; i32.const 8389648 ]
  running instr : call 1
  stack         : [ i32.const 0 ]
  stack         : [ i32.const 0 ; i32.const 8389632 ]
  running instr : i32.store offset=8 align=4
  stack         : [  ]
  running instr : local.get 0
  stack         : [ i32.const 8389632 ]
  running instr : i32.load offset=8 align=4
  stack         : [ i32.const 0 ]
  running instr : call 6
  calling func  : func anonymous
  stack         : [  ]
  running instr : local.get 0
  stack         : [ i32.const 0 ]
  running instr : call 2
  stack         : [ i32.const 0 ]
  running instr : drop
  stack         : [  ]
  stack         : [  ]
  running instr : i32.const 0
  stack         : [ i32.const 0 ]
  running instr : call 0
  Assertion failure was correctly reached

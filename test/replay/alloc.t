  $ owi c alloc.c -o alloc.wasm -O0 --no-value
  owi: [ERROR] Assert failure: false
  model {
    symbol symbol_0 i32
  }
  owi: [ERROR] Reached problem!
  [13]

  $ owi replay --replay-file alloc.scfg alloc.wasm --entry-point=main -v
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 1024
  owi: [INFO] stack         : [ i32.const 1024 ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 1024 ]
  owi: [INFO] running instr : i32.const 4
  owi: [INFO] stack         : [ i32.const 4 ; i32.const 0 ; i32.const 1024 ]
  owi: [INFO] running instr : memory.init 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : data.drop 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 14
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 0 ]
  owi: [INFO] running instr : call 13
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 12
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : global.get 0
  owi: [INFO] stack         : [ i32.const 8389648 ]
  owi: [INFO] running instr : i32.const 16
  owi: [INFO] stack         : [ i32.const 16 ; i32.const 8389648 ]
  owi: [INFO] running instr : i32.sub
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : global.set 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : call 11
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 3
  owi: [INFO] stack         : [ i32.const 9 ; i32.const 8389632 ]
  owi: [INFO] running instr : i32.store offset=12 align=4
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : i32.load offset=12 align=4
  owi: [INFO] stack         : [ i32.const 9 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ i32.const 10 ; i32.const 9 ]
  owi: [INFO] running instr : i32.lt_s
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : call 4
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : i32.load offset=12 align=4
  owi: [INFO] stack         : [ i32.const 9 ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 9 ]
  owi: [INFO] running instr : i32.gt_s
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : call 4
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 8389632 ; i32.const 8389632 ]
  owi: [INFO] running instr : i32.load offset=12 align=4
  owi: [INFO] stack         : [ i32.const 9 ; i32.const 8389632 ]
  owi: [INFO] running instr : call 9
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : block
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 9 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 9 ; i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : i32.clz
  owi: [INFO] stack         : [ i32.const 28 ; i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const 31
  owi: [INFO] stack         : [ i32.const 31 ; i32.const 28 ; i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ i32.const 3 ; i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : local.tee 1
  owi: [INFO] stack         : [ i32.const 3 ; i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : i32.shl
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const 16
  owi: [INFO] stack         : [ i32.const 16 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32.const 3 ; i32.const 16 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const 5
  owi: [INFO] stack         : [ i32.const 5 ; i32.const 3 ; i32.const 16 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.lt_u
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 16 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : select
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.tee 1
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.load offset=1024 align=4
  owi: [INFO] stack         : [ i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.tee 2
  owi: [INFO] stack         : [ i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const -1
  owi: [INFO] stack         : [ i32.const -1 ; i32.const 8 ; i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 7 ; i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.tee 1
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.sub
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 0 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : select
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 2
  owi: [INFO] stack         : [ i32.const 8389648 ; i32.const 0 ; i32.const 0 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 8389648 ; i32.const 0 ]
  owi: [INFO] running instr : local.tee 1
  owi: [INFO] stack         : [ i32.const 8389648 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 9 ; i32.const 8389648 ; i32.const 0 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 8389657 ; i32.const 0 ]
  owi: [INFO] running instr : i32.store offset=1024 align=4
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32.const 8389648 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 9 ; i32.const 8389648 ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 8389632 ]
  owi: [INFO] running instr : i32.store offset=8 align=4
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : i32.load offset=8 align=4
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : call 10
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : call 1
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : drop
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : call 2
  owi: [INFO] scopes : [symbol symbol_0]
  Assertion failure was correctly reached!

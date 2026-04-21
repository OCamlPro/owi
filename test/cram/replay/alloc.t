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
  owi: [INFO] running instr : i32.const 1024 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1024 ]
  owi: [INFO] running instr : i32.const 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 1024 ]
  owi: [INFO] running instr : i32.const 4 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 4 ; i32.const 0 ; i32.const 1024 ]
  owi: [INFO] running instr : memory.init 0 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : data.drop 0 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 14 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : i32.const 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 0 ]
  owi: [INFO] running instr : call 13 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 12 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : global.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389648 ]
  owi: [INFO] running instr : i32.const 16 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 16 ; i32.const 8389648 ]
  owi: [INFO] running instr : i32.sub (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : local.tee 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : global.set 0 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : call 11 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 3 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 9 ; i32.const 8389632 ]
  owi: [INFO] running instr : i32.store offset=12 align=4 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : i32.load offset=12 align=4 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 9 ]
  owi: [INFO] running instr : i32.const 10 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 10 ; i32.const 9 ]
  owi: [INFO] running instr : i32.lt_s (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : i32.const 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] running instr : i32.and (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : call 4 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : i32.load offset=12 align=4 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 9 ]
  owi: [INFO] running instr : i32.const 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 9 ]
  owi: [INFO] running instr : i32.gt_s (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : i32.const 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] running instr : i32.and (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : call 4 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389632 ; i32.const 8389632 ]
  owi: [INFO] running instr : i32.load offset=12 align=4 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 9 ; i32.const 8389632 ]
  owi: [INFO] running instr : call 9 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : block (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 9 ]
  owi: [INFO] running instr : br_if 0 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : i32.const 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 9 ; i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : i32.clz (executed 0 times)
  owi: [INFO] stack         : [ i32.const 28 ; i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const 31 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 31 ; i32.const 28 ; i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : i32.xor (executed 0 times)
  owi: [INFO] stack         : [ i32.const 3 ; i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : local.tee 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 3 ; i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : i32.shl (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const 16 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 16 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 3 ; i32.const 16 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const 5 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 5 ; i32.const 3 ; i32.const 16 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.lt_u (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 16 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : select (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.tee 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.load offset=1024 align=4 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.tee 2 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const -1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const -1 ; i32.const 8 ; i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.add (executed 0 times)
  owi: [INFO] stack         : [ i32.const 7 ; i32.const 8389648 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.and (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.tee 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.sub (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 0 ; i32.const 8 ; i32.const 0 ]
  owi: [INFO] running instr : select (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 2 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389648 ; i32.const 0 ; i32.const 0 ]
  owi: [INFO] running instr : i32.add (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389648 ; i32.const 0 ]
  owi: [INFO] running instr : local.tee 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389648 ; i32.const 0 ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 9 ; i32.const 8389648 ; i32.const 0 ]
  owi: [INFO] running instr : i32.add (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389657 ; i32.const 0 ]
  owi: [INFO] running instr : i32.store offset=1024 align=4 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389648 ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 9 ; i32.const 8389648 ]
  owi: [INFO] running instr : call 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 8389632 ]
  owi: [INFO] running instr : i32.store offset=8 align=4 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 8389632 ]
  owi: [INFO] running instr : i32.load offset=8 align=4 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : call 10 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : call 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : drop (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : call 2 (executed 0 times)
  owi: [INFO] scopes : [symbol symbol_0]
  Assertion failure was correctly reached!

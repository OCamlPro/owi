  $ owi wasm abs non_rec.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {2} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {3} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : drop

  $ owi wasm abs rec.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [ERROR] Too many recursive calls

  $ owi wasm abs ref.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : ref.func 18
  owi: [INFO] stack         : [ ref ... ; i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call_ref 0
  owi: [ERROR] hereo
  owi: [ERROR] hereo
  owi: [INFO] calling func  : func incr
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.add
  owi: [ERROR] hereo
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : drop

  $ owi wasm abs indirect.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ; i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : table.init 0 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : elem.drop 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32 {0} ; i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call_indirect 0  (param i32) (result i32)
  owi: [INFO] calling func  : func f
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.eqz
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : if
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : block
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : drop

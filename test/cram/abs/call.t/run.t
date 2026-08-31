  $ owi abs non_rec.wat -v
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

  $ owi abs rec.wat -v
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
  owi: [ERROR] Too many recursive calls


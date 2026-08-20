  $ owi abs div_zero.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 19
  owi: [INFO] calling func  : func start
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ i32 {42} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 [--..--] ; i32 {42} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.div_s
  owi: [INFO] stack         : [ i32 [-42..42] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]) ]
  owi: [INFO] running instr : drop
  owi: [WARNING] Possible division by zero for expression:(uuid: 4) i32.div_s

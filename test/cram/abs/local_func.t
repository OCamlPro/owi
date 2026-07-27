  $ owi abs local_func.wat -v
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
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func add
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {42}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {42} ]
  owi: [INFO] locals        : [ (0 -> i32 {42}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {42} ; i32 {42} ]
  owi: [INFO] locals        : [ (0 -> i32 {42}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {84} ]
  owi: [INFO] locals        : [ (0 -> i32 {42}) ]
  owi: [INFO] running instr : return
  owi: [INFO] stack         : [ i32 {84} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : return

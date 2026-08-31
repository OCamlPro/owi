  $ owi abs assume.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 20
  owi: [INFO] calling func  : func start
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32 {0} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.gt_s
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]) ]
  owi: [INFO] running instr : call 7
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 [1..0x7FFFFFFF] ; i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.div_s
  owi: [INFO] stack         : [ i32 [0..100] ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32 {0} ; i32 [0..100] ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.gt_s
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : call 7
  [Single_value_abstraction.Noop] Warning: No backpropagation for 'bisdiv'owi: [INFO] Passed division by zero check for expression:(uuid: 7) i32.div_s
  



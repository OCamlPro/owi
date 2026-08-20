  $ owi abs if_join.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 21
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0});  (1 -> i32 {0}) ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 {0});  (1 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32 {0} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}) ]
  owi: [INFO] running instr : i32.gt_s
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}) ]
  owi: [INFO] running instr : if (result i32)
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]);  (1 -> i32 {0}) ]
  owi: [INFO] running instr : block (result i32)
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]);  (1 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 [1..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]);  (1 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ i32 {10} ; i32 [1..0x7FFFFFFF] ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]);  (1 -> i32 {0}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..0]);  (1 -> i32 {0}) ]
  owi: [INFO] running instr : block (result i32)
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..0]);  (1 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32 [0..0x80000009] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]);  (1 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 1

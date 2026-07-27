  $ owi abs fact.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0});  (1 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {0});  (1 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : block $done
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : loop $continue
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ; i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {2} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : i32.mul
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {1}) ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ; i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : i32.sub
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {2});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : br 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32 {1} ; i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [ (0 -> i32 {1});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {1});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32 {2} ]
  owi: [INFO] locals        : [ (0 -> i32 {1});  (1 -> i32 {2}) ]
  owi: [INFO] running instr : drop



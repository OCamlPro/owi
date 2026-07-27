  $ owi abs add.wat -v
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
  owi: [INFO] running instr : i32.const 28
  owi: [INFO] stack         : [ i32 {28} ; i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.sub
  owi: [INFO] stack         : [ i32 {14} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 47
  owi: [INFO] stack         : [ i32 {47} ; i32 {14} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {61} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i64.extend_i32_s
  owi: [INFO] stack         : [ i64 {61} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i64.const 100
  owi: [INFO] stack         : [ i64 {100} ; i64 {61} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i64.add
  owi: [INFO] stack         : [ i64 {161} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i64.const 200
  owi: [INFO] stack         : [ i64 {200} ; i64 {161} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i64.gt_s
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : return



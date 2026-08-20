  $ owi abs globals.wat -v
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
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : if
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : block
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : global.set 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : block
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 54
  owi: [INFO] stack         : [ i32 {54} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : global.set 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : global.get 0
  owi: [INFO] stack         : [ i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : drop



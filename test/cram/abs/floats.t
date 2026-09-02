  $ owi wasm abs floats.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f32.const 4
  owi: [INFO] stack         : [ f32 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f32.const 100
  owi: [INFO] stack         : [ f32 ... ; f32 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f32.sqrt
  owi: [INFO] stack         : [ f32 ... ; f32 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f32.add
  owi: [INFO] stack         : [ f32 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.trunc_f32_s
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f32.convert_i32_s
  owi: [INFO] stack         : [ f32 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f64.promote_f32
  owi: [INFO] stack         : [ f64 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f64.const 4_294_967_296
  owi: [INFO] stack         : [ f64 ... ; f64 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f64.add
  owi: [INFO] stack         : [ f64 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f64.const 4_294_967_295
  owi: [INFO] stack         : [ f64 ... ; f64 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : f64.sub
  owi: [INFO] stack         : [ f64 ... ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : return
